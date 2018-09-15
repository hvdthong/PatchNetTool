import argparse


def read_args():
    parser = argparse.ArgumentParser()
    # Training our model
    parser.add_argument('-train', action='store_true', help='training our model')
    # Number of parameters
    args = parser.parse_args()
    return args


if __name__ == '__main__':
    # input_option = read_args()
    # print input_option
    # print type(input_option)
    # print input_option.train

    data_dir = "./data/data_small.text"
