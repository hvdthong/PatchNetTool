from extract_commit import commits_index, commit_id, commit_stable, \
    commit_msg, commit_date, commit_code, commit_msg_new, commit_code_new, commit_date_july, commit_msg_july, \
    commit_code_july
from filter_commit import filter_number_code_file, filter_number_code_hunk, filter_loc_hunk, filter_loc_len
import os


def load_file(path_file):
    lines = list(open(path_file, "r").readlines())
    lines = [l.strip() for l in lines]
    return lines


def write_file(path_file, data):
    split_path = path_file.split("/")
    path_ = split_path[:len(split_path) - 1]
    path_ = "/".join(path_)

    if not os.path.exists(path_):
        os.makedirs(path_)
    with open(path_file, 'w') as out_file:
        for line in data:
            # write line to output file
            out_file.write(str(line))
            out_file.write("\n")
        out_file.close()


def commit_info(commit):
    id = commit_id(commit)
    stable = commit_stable(commit)
    date = commit_date(commit)
    msg = commit_msg(commit)
    code = commit_code(commit)
    return id, stable, date, msg, code


def commit_info_july(commit):
    id = commit_id(commit)
    stable = commit_stable(commit)
    date = commit_date_july(commit)
    msg = commit_msg_july(commit)
    code = commit_code_july(commit)
    return id, stable, date, msg, code


def commit_info_new(commit):
    id = commit_id(commit)
    stable = commit_stable(commit)
    msg = commit_msg_new(commit)
    code = commit_code_new(commit)
    return id, stable, msg, code


def extract_commit(path_file):
    # extract commit from july data
    commits = load_file(path_file=path_file)
    indexes = commits_index(commits=commits)
    dicts = list()
    for i in xrange(0, len(indexes)):
        dict = {}
        if i == len(indexes) - 1:
            id, stable, date, msg, code = commit_info_july(commits[indexes[i]:])
        else:
            id, stable, date, msg, code = commit_info_july(commits[indexes[i]:indexes[i + 1]])
        dict["id"] = id
        dict["stable"] = stable
        dict["date"] = date
        dict["msg"] = msg
        dict["code"] = code
        dicts.append(dict)
    return dicts


def interset(lists):
    result = set(lists[0])
    for i in xrange(1, len(lists)):
        result = result.intersection(lists[i])
    return list(result)


def get_commits(commits, ids):
    new_commits = []
    for c in commits:
        if c["id"] in ids:
            new_commits.append(c)
    return new_commits


def filtering_commit(commits, num_file, num_hunk, num_loc, size_line):
    code_file_ids = filter_number_code_file(commits=commits, num_file=num_file)
    code_hunk_ids = filter_number_code_hunk(commits=commits, num_hunk=num_hunk)
    loc_hunk_ids = filter_loc_hunk(commits=commits, num_loc=num_loc)
    loc_len_ids = filter_loc_len(commits=commits, size_line=size_line)
    all_ids = [code_file_ids] + [code_hunk_ids] + [loc_hunk_ids] + [loc_len_ids]
    intersect_ids = interset(all_ids)
    new_commits = get_commits(commits=commits, ids=intersect_ids)
    return new_commits


def filtering_commit_union(commits, num_file, num_hunk, num_loc, size_line):
    code_file_ids = filter_number_code_file(commits=commits, num_file=num_file)
    code_hunk_ids = filter_number_code_hunk(commits=commits, num_hunk=num_hunk)
    loc_hunk_ids = filter_loc_hunk(commits=commits, num_loc=num_loc)
    loc_len_ids = filter_loc_len(commits=commits, size_line=size_line)
    all_ids = [code_file_ids] + [code_hunk_ids] + [loc_hunk_ids] + [loc_len_ids]
    all_ids = interset(all_ids)
    return all_ids


if __name__ == "__main__":
    path_data = "./data/data_small.text"
    commits_ = extract_commit(path_file=path_data)
    nfile, nhunk, nline, nleng = 1, 8, 10, 120
    total_ids = filtering_commit_union(commits=commits_, num_file=nfile, num_hunk=nhunk, num_loc=nline, size_line=nleng)
    print total_ids
