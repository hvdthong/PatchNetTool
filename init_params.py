import tensorflow as tf


def model_parameters(msg_length, code_length, code_line, code_hunk, code_file,
                     embedding_dim_text, filter_sizes, num_filters, hidden_units, dropout_keep_prob, l2_reg_lambda,
                     learning_rate, batch_size, num_epochs, evaluate_every, checkpoint_every, num_checkpoints):
    # Data loading
    tf.flags.DEFINE_integer("msg_length", msg_length, "Max length of message in commits")
    tf.flags.DEFINE_integer("code_length", code_length, "Max length of code in one line in commits")
    tf.flags.DEFINE_integer("code_line", code_line, "Max line of code in one hunk in commits")
    tf.flags.DEFINE_integer("code_hunk", code_hunk, "Max hunk of code in one file in commits")
    tf.flags.DEFINE_integer("code_file", code_file, "Max file of code in one in commits")

    # Model Hyperparameters for commit message (text) and commit code
    tf.flags.DEFINE_integer("embedding_dim_text", embedding_dim_text,
                            "Dimensionality of character embedding for text in commit message")
    # num_filters, filter_sizes is used for both commit message (text) and commit code
    tf.flags.DEFINE_string("filter_sizes", filter_sizes, "Comma-separated filter sizes")
    tf.flags.DEFINE_integer("num_filters", num_filters, "Number of filters per filter size")

    # Hypverparameters after we construct embedding vectors for commit message and commit code
    tf.flags.DEFINE_integer("hidden_units", hidden_units, "Number of hidden layer units (default: 100)")

    # Parameters for training our model
    tf.flags.DEFINE_float("dropout_keep_prob", dropout_keep_prob, "Dropout keep probability (default: 0.5)")
    tf.flags.DEFINE_float("l2_reg_lambda", l2_reg_lambda, "L2 regularization lambda (default: 0.0)")
    tf.flags.DEFINE_float("learning_rate", learning_rate, "Learning rate for optimization techniques")

    # Setup parameters for our model: batch_size, num_epochs, etc.
    tf.flags.DEFINE_integer("batch_size", batch_size, "Batch Size (default: 64)")
    tf.flags.DEFINE_integer("num_epochs", num_epochs, "Number of training epochs (default: 200)")
    tf.flags.DEFINE_integer("evaluate_every", evaluate_every,
                            "Evaluate model on dev set after this many steps (default: 100)")
    tf.flags.DEFINE_integer("checkpoint_every", checkpoint_every, "Save model after this many steps (default: 100)")
    tf.flags.DEFINE_integer("num_checkpoints", num_checkpoints, "Number of checkpoints to store (default: 5)")

    # Misc Parameters# Misc Parameters  # default
    tf.flags.DEFINE_boolean("allow_soft_placement", True, "Allow device soft device placement")
    tf.flags.DEFINE_boolean("log_device_placement", False, "Log placement of ops on devices")
    return tf


def print_params(tf):
    FLAGS = tf.flags.FLAGS
    FLAGS._parse_flags()
    print("Parameters:")
    for attr, value in sorted(FLAGS.__flags.items()):
        print("{}={}".format(attr.upper(), value))
    print("")
