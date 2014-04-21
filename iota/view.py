import iota
import json
import logging
import utils

def view(database, docid):
    """fill this in
    """
    try:
        paper = database.get_document(docid)
    except:
        return None
    else:
        v = {'view': json.loads(paper.get_data())}
        v['view']['docid'] = docid

    return [utils.to_sexp(v)]
