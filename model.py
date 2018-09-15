import tensorflow as tf


class PatchNet(object):
    # ==================================================
    # ==================================================
    def __init__(self, max_msg_length, max_code_length, max_code_line, max_code_hunk, vocab_size_text,
                 vocab_size_code, embedding_size_text, filter_sizes, num_filters, l2_reg_lambda, num_classes,
                 hidden_units):
        self.max_msg_length = max_msg_length
        self.max_code_length = max_code_length
        self.max_code_line = max_code_line
        self.max_code_hunk = max_code_hunk
        self.vocab_size_text = vocab_size_text
        self.vocab_size_code = vocab_size_code
        self.embedding_size_text = embedding_size_text
        self.filter_sizes = filter_sizes
        self.num_filters = num_filters
        self.l2_reg_lambda = l2_reg_lambda
        self.num_classes = num_classes
        self.hidden_units = hidden_units

    def _create_place_holder(self):
        # Placeholders for input and dropout
        self.input_msg = tf.placeholder(tf.int32, [None, self.max_msg_length], name='input_msg')
        self.input_addedcode = tf.placeholder(tf.int32,
                                              [None, self.max_code_hunk, self.max_code_line, self.max_code_length],
                                              name='input_addedcode')
        self.input_removedcode = tf.placeholder(tf.int32,
                                                [None, self.max_code_hunk, self.max_code_line, self.max_code_length],
                                                name='input_removedcode')
        # Label data
        # self.input_y = tf.placeholder(tf.float32, [None, 1], name="input_y")
        self.input_y = tf.placeholder(tf.float32, [None, self.num_classes], name="input_y")

        # loss value
        self.l2_loss = tf.constant(0.0)  # we don't use regularization in our model
        self.num_filters_total = self.num_filters * len(self.filter_sizes)

        # Create a placeholder for dropout. They depends on the model that we are going to use
        self.dropout_keep_prob = tf.placeholder(tf.float32, name='dropout_keep_prob')

    # ==================================================
    # ==================================================
    # create an embedding vector for each word in commit message and commit code
    def _create_embedding_msg_layer(self):
        # Embedding commit message layer
        with tf.device('/cpu:0'), tf.name_scope("embedding_msg"):
            self.W_msg = tf.Variable(
                tf.random_uniform([self.vocab_size_text, self.embedding_size_text], -1.0, 1.0),
                name="W_msg")

    def _create_embedding_code_layer(self):
        # Embedding code layer
        # indices = [i for i in xrange(self.vocab_size_code)]
        # depth = self.vocab_size_code
        # with tf.device('/cpu:0'), tf.name_scope("embedding_code"):
        #     self.W_code = tf.one_hot(indices=indices, depth=depth, name="W_code")

        with tf.device('/cpu:0'), tf.name_scope("embedding_code"):
            self.W_code = tf.Variable(
                tf.random_uniform([self.vocab_size_code, self.embedding_size_text], -1.0, 1.0),
                name="W_code")

    # ==================================================
    # ==================================================
    # mapping to an embedding vector to commit message and commit code
    def _create_embedding_chars_layer(self, W, input):
        embedded_chars = tf.nn.embedding_lookup(W, input)
        return tf.expand_dims(embedded_chars, -1)

    # create embedding layer for text
    def _create_embedding_chars_msg_layer(self):
        self.embedded_chars_expanded_msg = self._create_embedding_chars_layer(W=self.W_msg,
                                                                              input=self.input_msg)

    # create embedding layer for code
    def _create_embedding_chars_code_layer(self):
        self.embedded_chars_expanded_addedcode = self._create_embedding_chars_layer(W=self.W_code,
                                                                                    input=self.input_addedcode)
        self.embedded_chars_expanded_removedcode = self._create_embedding_chars_layer(W=self.W_code,
                                                                                      input=self.input_removedcode)

    # ==================================================
    # ==================================================
    # pooling for 2d metrics
    def pool_outputs_2d(self, embedded_chars_expanded, W, b, max_length, filter_size):
        conv = tf.nn.conv2d(
            embedded_chars_expanded,
            W,
            strides=[1, 1, 1, 1],
            padding="VALID",
            name="conv")

        # Apply nonlinearity -- using elu
        h = tf.nn.elu(tf.nn.bias_add(conv, b), name="elu")

        # Maxpooling over the outputs
        pooled = tf.nn.max_pool(
            h,
            ksize=[1, max_length - filter_size + 1, 1, 1],
            strides=[1, 1, 1, 1],
            padding='VALID',
            name="pool")
        return pooled

    def h_pool_2d(self, num_filters_total, pooled_outputs):
        h_pool_ = tf.reshape(tf.concat(pooled_outputs, 3), [-1, num_filters_total])
        return h_pool_

    # create weight embedding layer for text and do pooling for text
    def _create_weight_conv_msg_layer(self):
        self.w_filter_text, self.b_filter_text = [], []
        for i, filter_size in enumerate(self.filter_sizes):
            with tf.device("/cpu:" + str(filter_size)):
                with tf.name_scope("weight-conv-maxpool-text-%s" % filter_size):
                    filter_shape_text = [filter_size, self.embedding_size_text, 1, self.num_filters]
                    # Convolution Layer
                    w = tf.Variable(tf.truncated_normal(filter_shape_text, stddev=0.1), name="W_filter_text")
                    b = tf.Variable(tf.constant(0.1, shape=[self.num_filters]), name="b")
                    self.w_filter_text.append(w)
                    self.b_filter_text.append(b)

    def _create_conv_maxpool_2d_layer(self, filter_sizes, embedded_chars_expanded, W, b, max_msg_length):
        pooled_outputs_text = []
        for i, filter_size in enumerate(filter_sizes):
            with tf.device("/cpu:" + str(filter_size)):
                pooled_outputs_text.append(self.pool_outputs_2d(embedded_chars_expanded=embedded_chars_expanded,
                                                                W=W[i], b=b[i], max_length=max_msg_length,
                                                                filter_size=filter_size))
        return pooled_outputs_text

    def _create_conv_maxpool_msg_layer(self):
        pooled_outputs_text = self._create_conv_maxpool_2d_layer(filter_sizes=self.filter_sizes,
                                                                 embedded_chars_expanded=self.embedded_chars_expanded_msg,
                                                                 W=self.w_filter_text, b=self.b_filter_text,
                                                                 max_msg_length=self.max_msg_length)
        self.pooled_outputs_text = self.h_pool_2d(num_filters_total=len(self.filter_sizes) * self.num_filters,
                                                  pooled_outputs=pooled_outputs_text)

    # ==================================================
    # ==================================================
    # create weight embedding layer for each line in commit code
    def _create_embedding_code_line(self, embedded_chars_expanded):
        return tf.reduce_mean(embedded_chars_expanded, 3)

    def _create_embedding_addedcode_line(self):
        self.embedded_chars_expanded_addedcode_line = self._create_embedding_code_line(
            embedded_chars_expanded=self.embedded_chars_expanded_addedcode)

    def _create_embedding_removedcode_line(self):
        self.embedded_chars_expanded_removedcode_line = self._create_embedding_code_line(
            embedded_chars_expanded=self.embedded_chars_expanded_removedcode)

    # ==================================================
    # ==================================================
    # create weight embedding layer for each hunk in commit code
    def _create_weight_conv_code_layer(self):
        self.w_filter_code, self.b_filter_code = list(), list()
        for i, filter_size in enumerate(self.filter_sizes):
            with tf.device("/cpu:" + str(filter_size)):
                with tf.name_scope("weight-conv-maxpool-lines-%s" % filter_size):
                    # filter_shape_lines_code = [1, filter_size, self.vocab_size_code, 1, self.num_filters]
                    filter_shape_lines_code = [1, filter_size, self.embedding_size_text, 1, self.num_filters]
                    # Convolution Layer
                    w = tf.Variable(tf.truncated_normal(filter_shape_lines_code, stddev=0.1),
                                    name="W_filter_lines_%s" % filter_size)
                    b = tf.Variable(tf.constant(0.1, shape=[self.num_filters]), name="b_filter_lines_%s" % filter_size)
                    self.w_filter_code.append(w)
                    self.b_filter_code.append(b)
        return self.w_filter_code, self.b_filter_code

    # ==================================================
    # create weight embedding layer for each hunk in commit code
    def pool_outputs_3d(self, embedded_chars_expanded, W, b, max_length, filter_size):
        conv = tf.nn.conv3d(
            embedded_chars_expanded,
            W,
            strides=[1, 1, 1, 1, 1],
            padding="VALID",
            name="conv")

        # Apply nonlinearity -- using elu
        h = tf.nn.elu(tf.nn.bias_add(conv, b), name="elu")
        pooled = tf.nn.max_pool3d(
            h,
            ksize=[1, 1, max_length - filter_size + 1, 1, 1],
            strides=[1, 1, 1, 1, 1],
            padding='VALID',
            name="pool")
        return pooled

    def h_pool_3d(self, num_filters_total, pooled_outputs, height):
        pooled_outputs = tf.concat(pooled_outputs, 4)
        h_pool_ = tf.reshape(pooled_outputs, [-1, height, num_filters_total])
        return h_pool_

    def _create_conv_maxpool_3d_layer(self, filter_sizes, embedded_chars, W, b, max_length):
        pool_outputs_linescode = []
        for i, filter_size in enumerate(filter_sizes):
            with tf.device("/cpu:" + str(filter_size)):
                # convolution + maxpool for text
                pool_outputs_linescode.append(
                    self.pool_outputs_3d(embedded_chars_expanded=embedded_chars, W=W[i], b=b[i],
                                         filter_size=filter_size, max_length=max_length))
        return pool_outputs_linescode

    # ==================================================
    # create an embedding vectors for each hunk in commit code
    def _create_conv_maxpool_hunk_addedcode_layer(self):
        pooled_outputs_hunk_addedcode = self._create_conv_maxpool_3d_layer(filter_sizes=self.filter_sizes,
                                                                           embedded_chars=self.embedded_chars_expanded_addedcode_line,
                                                                           W=self.w_filter_code,
                                                                           b=self.b_filter_code,
                                                                           max_length=self.max_code_line)
        self.pooled_outputs_hunk_addedcode = self.h_pool_3d(
            num_filters_total=len(self.filter_sizes) * self.num_filters,
            pooled_outputs=pooled_outputs_hunk_addedcode,
            height=self.max_code_hunk)
        # self.pooled_outputs_hunk_addedcode = tf.contrib.layers.flatten(self.pooled_outputs_hunk_addedcode)
        # self.pooled_outputs_hunk_addedcode = tf.reshape(self.pooled_outputs_hunk_addedcode, [None, -1])

    def _create_conv_maxpool_hunk_removedcode_layer(self):
        pooled_outputs_hunk_removedcode = self._create_conv_maxpool_3d_layer(filter_sizes=self.filter_sizes,
                                                                             embedded_chars=self.embedded_chars_expanded_removedcode_line,
                                                                             W=self.w_filter_code,
                                                                             b=self.b_filter_code,
                                                                             max_length=self.max_code_line)
        self.pooled_outputs_hunk_removedcode = self.h_pool_3d(
            num_filters_total=len(self.filter_sizes) * self.num_filters,
            pooled_outputs=pooled_outputs_hunk_removedcode,
            height=self.max_code_hunk)

    # ==================================================
    # create an embedding vectors for added code and removed code in commit code
    def _create_embedding_addedcode(self):
        self.embedding_addedcode_layer = tf.contrib.layers.flatten(self.pooled_outputs_hunk_addedcode)

    def _create_embedding_removedcode(self):
        self.embedding_removedcode_layer = tf.contrib.layers.flatten(self.pooled_outputs_hunk_removedcode)

    # ==================================================
    # ==================================================
    # Fusion layer for text and commit code
    def _create_fusion_layer(self):
        self.fusion_layer = tf.concat(
            [self.pooled_outputs_text, self.embedding_addedcode_layer, self.embedding_removedcode_layer], 1)

    # Fusion layer for text (message) in commit code
    def _create_fusion_text_layer(self):
        self.fusion_layer = self.pooled_outputs_text

    # Fusion layer for code in commit code
    def _create_fusion_code_layer(self):
        self.fusion_layer = tf.concat(
            [self.embedding_addedcode_layer, self.embedding_removedcode_layer], 1)

    # ==================================================
    # adding drop_out
    def _adding_dropout_fusion_layer(self):
        self.fusion_layer_dropout = tf.nn.dropout(self.fusion_layer, self.dropout_keep_prob)

    # ==================================================
    # making weight to connect fusion layer -> hidden layer -> output layer
    # later

    # ==================================================
    # making weight to connect fusion layer -> output layer
    def _create_weight_fusion_layer(self):
        with tf.name_scope("weight_fusion"):
            self.W_fusion = tf.get_variable(
                "W_fusion",
                shape=[self.fusion_layer_dropout.get_shape()[1], self.num_classes],
                initializer=tf.contrib.layers.xavier_initializer())
            self.b_fusion = tf.Variable(tf.constant(0.1, shape=[self.num_classes]), name="b")
            self.l2_loss += tf.nn.l2_loss(self.W_fusion)
            self.l2_loss += tf.nn.l2_loss(self.b_fusion)

    # ==================================================
    # create output layer (score and prediction)
    def _create_output_layer(self):
        with tf.name_scope("output"):
            self.scores = tf.nn.xw_plus_b(self.fusion_layer_dropout, self.W_fusion, self.b_fusion, name="scores")
            # self.predictions = tf.sigmoid(self.scores, name="pred_prob")
            self.predictions = tf.argmax(self.scores, 1, name="predictions")

    # ==================================================
    # create output layer (score and prediction)
    def _create_loss_function(self):
        with tf.name_scope("loss"):
            # losses = tf.nn.sigmoid_cross_entropy_with_logits(logits=self.scores, labels=self.input_y)
            losses = tf.nn.softmax_cross_entropy_with_logits(logits=self.scores, labels=self.input_y)
            self.loss = tf.reduce_mean(losses) + self.l2_reg_lambda * self.l2_loss

    # check the accuracy of out predicted instances
    def _measure_accuracy(self):
        with tf.name_scope("accuracy"):
            # self.pred_label = tf.to_int64(self.predictions >= 0.5, name="pred_labels")
            # correct_predictions = tf.equal(self.pred_label, tf.argmax(self.input_y, 1))
            correct_predictions = tf.equal(self.predictions, tf.argmax(self.input_y, 1))
            self.accuracy = tf.reduce_mean(tf.cast(correct_predictions, "float"), name="accuracy")
            # self.accuracy = tf.metrics.accuracy(labels=self.input_y, predictions=self.pred_label)

    # ==================================================
    # ==================================================
    def build_graph(self, model):
        if model == "all":
            self._create_place_holder()
            self._create_embedding_msg_layer()
            self._create_embedding_chars_msg_layer()
            self._create_weight_conv_msg_layer()
            self._create_conv_maxpool_msg_layer()
            self._create_embedding_code_layer()
            self._create_embedding_chars_code_layer()
            self._create_embedding_addedcode_line()
            self._create_embedding_removedcode_line()
            self._create_weight_conv_code_layer()
            self._create_conv_maxpool_hunk_addedcode_layer()
            self._create_embedding_addedcode()
            self._create_conv_maxpool_hunk_removedcode_layer()
            self._create_embedding_removedcode()
            self._create_fusion_layer()
            self._adding_dropout_fusion_layer()
            self._create_weight_fusion_layer()
            self._create_output_layer()
            self._create_loss_function()
            self._measure_accuracy()
        elif model == "msg":
            self._create_place_holder()
            self._create_embedding_msg_layer()
            self._create_embedding_chars_msg_layer()
            self._create_weight_conv_msg_layer()
            self._create_conv_maxpool_msg_layer()
            self._create_fusion_text_layer()
            self._adding_dropout_fusion_layer()
            self._create_weight_fusion_layer()
            self._create_output_layer()
            self._create_loss_function()
            self._measure_accuracy()
        elif model == "code":
            self._create_place_holder()
            self._create_embedding_code_layer()
            self._create_embedding_chars_code_layer()
            self._create_embedding_addedcode_line()
            self._create_embedding_removedcode_line()
            self._create_weight_conv_code_layer()
            self._create_conv_maxpool_hunk_addedcode_layer()
            self._create_embedding_addedcode()
            self._create_conv_maxpool_hunk_removedcode_layer()
            self._create_embedding_removedcode()
            self._create_fusion_code_layer()
            self._adding_dropout_fusion_layer()
            self._create_weight_fusion_layer()
            self._create_output_layer()
            self._create_loss_function()
            self._measure_accuracy()
