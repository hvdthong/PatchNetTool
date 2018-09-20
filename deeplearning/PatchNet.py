import argparse
from ultis import extract_commit, reformat_commit_code
from train import train_model
from predict import predict_model


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
    parser.add_argument('--num_epochs', type=int, default=1, help='the number of epochs')
    parser.add_argument('--evaluate_every', type=int, default=500, help='evaluate model after this many steps')
    parser.add_argument('--checkpoint_every', type=int, default=1000, help='save model after this many steps')
    parser.add_argument('--num_checkpoints', type=int, default=100, help='the number of checkpoints to store')

    # Config tensorflow
    parser.add_argument('--allow_soft_placement', type=bool, default=True, help='allow device soft device placement')
    parser.add_argument('--log_device_placement', type=bool, default=False, help='Log placement of ops on devices')

    # Model
    parser.add_argument('--model_type', type=str, default='all', help='type of model for learning')
    parser.add_argument('--model', type=str, default='model', help='names of our model')
    return parser


if __name__ == '__main__':
    input_option = read_args().parse_args()
    input_help = read_args().print_help()

    commits = extract_commit(path_file=input_option.data_dir)
    commits = reformat_commit_code(commits=commits, num_file=1, num_hunk=input_option.code_hunk,
                                   num_loc=input_option.code_line, num_leng=input_option.code_length)

    # flag_train = True
    # if flag_train is True:
    #     train_model(commits=commits, params=input_option)

    flag_prediction = True
    if flag_prediction is True:
        predict_model(commits=commits, params=input_option)
