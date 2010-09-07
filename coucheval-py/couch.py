#! /usr/bin/python

import httplib, json

def prettyPrint(s):
    """Prettyprints the json reponse of an HTTPResponse object"""

    # HTTPResponse instance -> Python object -> str
    print json.dumps(json.loads(s.read()), sort_keys=True, indent=4)

class Couch:
    
                         
