import argparse
from ultis import extract_commit, reformat_commit_code
from init_params import model_parameters, print_params


def read_args():
    parser = argparse.ArgumentParser()
    # Training our model
    parser.add_argument('--train', action='store_true', help='training PatchNet model')
    parser.add_argument('--data_dir', type=str, default='./data/data_small.text',
                        help='the directory of our training data')

    # Predicting our data
    parser.add_argument('--predict', action='store_false', help='predicting testing data')

    # Number of parameters for reformatting commits
    parser.add_argument('--msg_length', type=int, default=512, help='the length of the commit message')
    parser.add_argument('--code_hunk', type=int, default=8, help='the number of hunks in commit code')
    parser.add_argument('--code_line', type=int, default=10, help='the number of LOC in each hunk of commit code')
    parser.add_argument('--code_length', type=int, default=120, help='the length of each LOC of commit code')

    # Number of parameters for PatchNet model
    parser.add_argument('--embedding_dim', type=int, default=32, help='the dimension of embedding vector')
    parser.add_argument('--filter_sizes', type=str, default='1, 2', help='the filter size of convolutional layers')
    parser.add_argument('--num_filters', type=int, default=32, help='the number of filters')
    parser.add_argument('--hidden_units', type=int, default=128, help='the number of nodes in hidden layers')
    parser.add_argument('--dropout_keep_prob', type=float, default=0.5, help='dropout for training PatchNet')
    parser.add_argument('--l2_reg_lambda', type=float, default=1e-5, help='regularization rate')
    parser.add_argument('--learning_rate', type=float, default=1e-4, help='learning rate')
    parser.add_argument('--batch_size', type=int, default=64, help='batch size')
    parser.add_argument('--num_epochs', type=int, default=25, help='the number of epochs')
    args = parser.parse_args()
    return args


def train_model(commits):
    print 'hello'


if __name__ == '__main__':
    input_option = read_args()
    commits = extract_commit(path_file=input_option.data_dir)
    new_commits = reformat_commit_code(commits=commits, num_file=1, num_hunk=input_option.code_hunk,
                                       num_loc=input_option.code_line, num_leng=input_option.code_length)
    print len(commits), len(new_commits)

    tf_ = model_parameters(msg_length=input_option.msg_length, code_hunk=input_option.code_hunk,
                           code_line=input_option.code_line, code_length=input_option.code_length,
                           embedding_dim=input_option.embedding_dim, filter_sizes=input_option.filter_sizes,
                           num_filters=input_option.num_filters, hidden_units=input_option.hidden_units,
                           dropout_keep_prob=input_option.dropout_keep_prob, l2_reg_lambda=input_option.l2_reg_lambda,
                           learning_rate=input_option.learning_rate, batch_size=input_option.batch_size,
                           num_epochs=input_option.num_epochs, evaluate_every=500, checkpoint_every=1000,
                           num_checkpoints=100)
    FLAGS_ = tf_.flags.FLAGS
    print_params(tf=tf_)
    print type(input_option.train), input_option.train
    print input_option.predict

    if input_option.train is True:
        print 'hello'
