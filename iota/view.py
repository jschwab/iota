import iota
import json
import logging
import sexpdata

def view(database, docid):
    """fill this in
    """
    try:
        paper = database.get_document(docid)
    except:
        return None
    else:
        v = {'view': json.loads(paper.get_data())}

    return sexpdata.dumps(v)
