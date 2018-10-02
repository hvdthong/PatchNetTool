from ultis import load_dict_file, mini_batches, write_file
from padding import padding_pred_commit
import os
import tensorflow as tf
import numpy as np


# correct solution:
def softmax(x):
    """Compute softmax values for each sets of scores in x."""
    # e_x = np.exp(x - np.max(x))
    # return e_x / e_x.sum(axis=0)  # only difference
    e_x_sum = np.sum(np.exp(x), axis=1)
    # return np.exp(x) / np.sum(np.exp(x), axis=0)
    return np.exp(x) / e_x_sum[:, None]


def predict_model(commits, params):
    path_dict = os.path.abspath(os.path.join(os.path.curdir, params.model))
    dict_msg = load_dict_file(path_file=path_dict + '/dict_msg.txt')
    dict_code = load_dict_file(path_file=path_dict + '/dict_code.txt')

    pad_msg, pad_added_code, pad_removed_code, labels = padding_pred_commit(commits=commits,
                                                                            params=params, dict_msg=dict_msg,
                                                                            dict_code=dict_code)
    # print pad_msg.shape, pad_added_code.shape, pad_removed_code.shape, labels.shape
    checkpoint_dir = path_dict
    checkpoint_file = tf.train.latest_checkpoint(checkpoint_dir)
    graph = tf.Graph()
    with graph.as_default():
        session_conf = tf.ConfigProto(
            allow_soft_placement=params.allow_soft_placement,
            log_device_placement=params.log_device_placement)
        sess = tf.Session(config=session_conf)
        with sess.as_default():
            # Load the saved meta graph and restore variables
            saver = tf.train.import_meta_graph("{}.meta".format(checkpoint_file))
            saver.restore(sess, checkpoint_file)

            # Get the placeholders from the graph by name
            input_msg = graph.get_operation_by_name("input_msg").outputs[0]
            input_addedcode = graph.get_operation_by_name("input_addedcode").outputs[0]
            input_removedcode = graph.get_operation_by_name("input_removedcode").outputs[0]
            dropout_keep_prob = graph.get_operation_by_name("dropout_keep_prob").outputs[0]

            # Tensors we want to evaluate
            scores = graph.get_operation_by_name("output/scores").outputs[0]

            # Generate batches for one epoch
            batches = mini_batches(X_msg=pad_msg, X_added_code=pad_added_code,
                                   X_removed_code=pad_removed_code,
                                   Y=labels, mini_batch_size=params.batch_size)
            # Collect the predictions here
            commits_scores = list()

            for batch in batches:
                batch_input_msg, batch_input_added_code, batch_input_removed_code, batch_input_labels = batch
                batch_scores = sess.run(scores,
                                        {input_msg: batch_input_msg, input_addedcode: batch_input_added_code,
                                         input_removedcode: batch_input_removed_code, dropout_keep_prob: 1.0})
                batch_scores = np.ravel(softmax(batch_scores)[:, [1]])
                commits_scores = np.concatenate([commits_scores, batch_scores])
            write_file(path_file=os.path.abspath(os.path.join(os.path.curdir)) + '/prediction.txt',
                       data=commits_scores)

