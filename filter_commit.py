def filter_number_code_file(commits, num_file):
    # check the number of code file in the commit code
    commit_id = list()
    for c in commits:
        if len(c["code"]) <= num_file:
            commit_id.append(c["id"])
        else:
            print 'testing'
    return commit_id


def filter_number_code_hunk(commits, num_hunk):
    commit_id = list()
    for c in commits:
        files = c["code"]
        cnt_hunk = list()
        for hunk in files:
            added_hunk, removed_hunk = hunk["added"].keys(), hunk["removed"].keys()
            cnt_hunk += added_hunk + removed_hunk
        if max(cnt_hunk) <= num_hunk:
            commit_id.append(c["id"])
    return commit_id


def get_loc_hunk(hunk_code):
    if len(hunk_code.keys()) > 0:
        loc_hunk_code = [len(hunk_code[k]) for k in hunk_code.keys()]
    else:
        loc_hunk_code = []
    return loc_hunk_code


def filter_loc_hunk(commits, num_loc):
    commit_id = list()
    for c in commits:
        files = c["code"]
        cnt_loc_hunk = list()
        for hunk in files:
            removed_code, added_code = hunk["removed"], hunk["added"]
            hunk_removed_code, hunk_added_code = get_loc_hunk(removed_code), get_loc_hunk(added_code)
            cnt_loc_hunk += hunk_removed_code + hunk_added_code
        if max(cnt_loc_hunk) <= num_loc:
            commit_id.append(c["id"])
    return commit_id


def get_loc_len(hunk_code):
    if len(hunk_code.keys()) > 0:
        len_loc_code = [len(l.split(",")) for k in hunk_code.keys() for l in hunk_code[k]]
    else:
        len_loc_code = []
    return len_loc_code


def filter_loc_len(commits, size_line):
    commit_id = list()
    for c in commits:
        files = c["code"]
        cnt_size_code = list()
        for hunk in files:
            removed_code, added_code = hunk["removed"], hunk["added"]
            len_loc_removed_code, len_loc_added_code = get_loc_len(removed_code), get_loc_len(added_code)
            cnt_size_code += len_loc_removed_code + len_loc_added_code
        if max(cnt_size_code) <= size_line:
            commit_id.append(c["id"])
    return commit_id
