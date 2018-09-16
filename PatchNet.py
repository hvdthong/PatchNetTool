import argparse
from ultis import extract_commit, reformat_commit_code


def read_args():
    parser = argparse.ArgumentParser()
    # Training our model
    parser.add_argument('--train', action='store_true', help='training PatchNet model')

    # Predicting our data
    parser.add_argument('--predict', action='store_true', help='predicting testing data')

    # Number of parameters for reformatting commits
    parser.add_argument('--msg_length', type=int, default=512, help='the length of the commit message')
    parser.add_argument('--code_hunk', type=int, default=8, help='the number of hunks in commit code')
    parser.add_argument('--code_line', type=int, default=10, help='the number of LOC in each hunk of commit code')
    parser.add_argument('--code_length', type=int, default=120, help='the length of each LOC of commit code')

    # Number of parameters for PatchNet model
    parser.add_argument('--embedding_dim', type=int, default=32, help='the dimension of embedding vector')
    parser.add_argument('--filter_sizes', type=str, default='1, 2', help='the filter size of convolutional layers')
    parser.add_argument('--hidden_units', type=int, default=128, help='the number of nodes in hidden layers')
    parser.add_argument('--dropout_keep_prob', type=float, default=0.5, help='dropout for training PatchNet')
    parser.add_argument('--l2_reg_lambda', type=float, default=1e-5, help='regularization rate')

    args = parser.parse_args()
    return args


def train_model(commits):
    print 'hello'


if __name__ == '__main__':
    input_option = read_args()
    print input_option
    # print type(input_option)
    # print input_option.train

    data_dir = "./data/data_small.text"
    commits = extract_commit(path_file=data_dir)
    nfile, nhunk, nloc, nleng = 1, 8, 10, 120
    new_commits = reformat_commit_code(commits=commits, num_file=nfile, num_hunk=nhunk, num_loc=nloc, num_leng=nleng)
    print len(new_commits)

    msg_length = 512
    code_length = 120
    code_line = 10
    code_hunk = 8
    embedding_dim_text = 32
    filter_sizes = '1, 2'
    num_filters = 32
    hidden_units = 128
    dropout_keep_prob = 0.5
    l2_reg_lambda = 1e-5
    learning_rate = 1e-4
    batch_size = 64
    num_epochs = 25
    evaluate_every = 500
    checkpoint_every = 1000
    exit()
