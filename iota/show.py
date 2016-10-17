import iota
import json
import logging
import utils
import subprocess
import os

def show(database, docid):
    """fill this in
    """
    try:
        paper = database.get_document(docid)
    except:
        return None
    else:
        data = json.loads(paper.get_data())

        with open(os.devnull, "w") as fnull:
            s = subprocess.Popen(["evince", os.path.join(data['path'], data['pdffile'])],
                                 stdout=fnull, stderr=fnull)
            pid = s.pid

    return [utils.to_sexp({'pid': pid})]
