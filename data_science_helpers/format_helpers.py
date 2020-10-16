import csv, io

def dict_as_csv(report_dict, row_label="class", key_of_dict_with_col_headers=None):
    """
    A dict of dicts becomes a csv

    row_label="class"
    {
        'one': {'A': 'abc', 'B': 'def'},
        'two': {'A': 'hij', 'B': 'klm'},
        'not a dict': 'I am ignored'
    }

    becomes:
    class ,A   ,B
    one   ,abc ,def
    two   ,hij ,klm

    """
    row_keys = list(report_dict.keys())
    if key_of_dict_with_col_headers is None:
        key_of_dict_with_col_headers = row_keys[0]
    col_keys = list(report_dict[key_of_dict_with_col_headers].keys())
    col_keys = [row_label] + col_keys
    output = io.StringIO()
    writer = csv.DictWriter(output,
        fieldnames=col_keys,
        delimiter=",",
        quoting=csv.QUOTE_MINIMAL)
    writer.writeheader()
    for key in row_keys:
        row=report_dict[key]
        if(type(row) is dict):
            row[row_label] = key
            row_to_write = {key: row[key] for key in col_keys}
            writer.writerow(row_to_write)
    return output.getvalue()