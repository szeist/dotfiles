#!/usr/bin/env python

from __future__ import print_function
import sys
import json

TODO_LIST_NAME = "To Do"
DOING_LIST_NAME = "Doing"

def _is_started(task):
    return 'start' in task

def _move_to_doing_list(task):
    dict(task)['intheamtrellolistname'] = DOING_LIST_NAME
    return task

def _move_to_todo_list(task):
    dict(task)['intheamtrellolistname'] = TODO_LIST_NAME
    return task

def main():
    sys.stdin.readline()
    new_task = json.loads(sys.stdin.readline())
    if _is_started(new_task):
        new_task = _move_to_doing_list(new_task)
    else:
        new_task = _move_to_todo_list(new_task)
    print(json.dumps(new_task))


if __name__ == '__main__':
    main()
