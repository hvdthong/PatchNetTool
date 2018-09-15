def reformat_file(commits, num_file):
    for c in commits:
        if len(c['code']) > num_file:
            code_files = c['code']
            c.update({'code': [code_files[0]]})
    return commits


def update_hunk(hunk, num_hunk):
    new_hunk = dict()
    for key in hunk:
        if key <= num_hunk:
            new_hunk[key] = hunk[key]
    return new_hunk


def reformat_hunk(commits, num_hunk):
    for c in commits:
        hunk = c['code'][0]
        added_hunk, removed_hunk = hunk['added'].keys(), hunk['removed'].keys()
        cnt_hunk = added_hunk + removed_hunk
        if max(cnt_hunk) > num_hunk:
            new_added_hunk, new_removed_hunk = update_hunk(hunk=hunk['added'], num_hunk=num_hunk), \
                                               update_hunk(hunk=hunk['removed'], num_hunk=num_hunk)
            hunk.update({'added': new_added_hunk})
            hunk.update({'removed': new_removed_hunk})
    return commits
