#!/usr/bin/env python3

import h5py
import numpy as np
import json
import sys
import argparse

def meta_dict(name: str, obj) -> dict:
    if isinstance(obj, h5py.Group):
        return {
            'type': 'group',
            'name': name,
            'children': []
        }
    elif isinstance(obj, h5py.Dataset):
        shape = "scalar"
        if (obj.shape is not None):
            if (len(obj.shape) > 0):
                shape = str(obj.shape)
            try: # calculate the data range
                datamin = np.nanmin(obj[:].reshape(-1))
                datamax = np.nanmax(obj[:].reshape(-1))
                if datamin == datamax:
                    datarange = f'{datamin:.4g}'
                else:
                    datarange = f'{datamin:.3g}:{datamax:.3g}'
            except: # take the 1st value if it's something weird
                datarange = str(obj[:][0])
        else:
            datarange = ""
            dtype     = ""
        return {'type': 'dataset',
                'name': name,
                'shape': shape,
                'range': datarange,
                'dtype': str(obj.dtype)}
    else:
        raise Exception(f"'{name}' is not a dataset or group")

class H5Instance:
    def __init__(self, filename: str):
        self.instance =  h5py.File(filename)

    def get_fields(self, root: str) -> dict:
        obj = self.instance[root]
        if not isinstance(obj, h5py.Group):
            raise Exception(f"'{root}' is not a group")
        fields = {}
        for cname, cobj in obj.items():
            fields[cname] = meta_dict(cname, cobj)
        return fields

    def preview_field(self, field: str) -> dict:
        obj = self.instance[field]
        meta = meta_dict(field, obj)
        np.set_printoptions(threshold=10, linewidth=sys.maxsize)
        if isinstance(obj, h5py.Group):
            # Return fields in group
            meta['data'] = str(list(obj.keys()))
        else:
            # Return data in field
            meta['data'] = str(obj[()])
        return meta

    def read_field(self, field: str) -> dict:
        obj = self.instance[field]
        meta = meta_dict(field, obj)
        linewidth = sys.maxsize
        if len(obj.shape) == 1:
            # Print 1d arrays vertically
            linewidth = 1
        np.set_printoptions(threshold=sys.maxsize, linewidth=linewidth)
        meta['data'] = str(obj[()])
        return meta

    def is_group(self, field: str) -> dict:
        true_or_false = False
        if self.is_field(field):
            obj = self.instance[field]
            if isinstance(obj, h5py.Group):
                return {"return": True}
        return {"return": true_or_false}

    def is_field(self, field: str) -> dict:
        true_or_false = field in self.instance
        return {"return": true_or_false}

    def get_attrs(self, root: str) -> dict:
        obj = self.instance[root]
        return {x[0]:str(x[1]) for x in obj.attrs.items()}

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('filepath'       , type=str, help='File to parse')
    parser.add_argument('--get-fields'   , type=str, help='Print fields within group')
    parser.add_argument('--get-attrs'    , type=str, help='Print attributes of parent to root')
    parser.add_argument('--preview-field', type=str, help='Print preview of requested field')
    parser.add_argument('--read-field'   , type=str, help='Print field data')
    parser.add_argument('--is-group'     , type=str, help='Print true if field is group')
    parser.add_argument('--is-field'     , type=str, help='Print true if field exists in file')
    args = parser.parse_args()

    inst = H5Instance(args.filepath)

    if args.get_fields:
        print(json.dumps(inst.get_fields(args.get_fields), indent=4))
    elif args.get_attrs:
        print(json.dumps(inst.get_attrs(args.get_attrs)))
    elif args.preview_field:
        print(json.dumps(inst.preview_field(args.preview_field)))
    elif args.read_field:
        print(json.dumps(inst.read_field(args.read_field)))
    elif args.is_group:
        print(json.dumps(inst.is_group(args.is_group)))
    elif args.is_field:
        print(json.dumps(inst.is_field(args.is_field)))
    exit(0)
