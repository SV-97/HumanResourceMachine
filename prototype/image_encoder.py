import zlib
import base64
from math import ceil

# a = zlib.compress(base64.encodestring(b"Hello World"))

def decode(blob):
    """ Decode a base64 byte-blob
    Args:
        blob (bytes): image blob (without ";")
    """
    next_mult = ceil(len(blob) / 4) * 4 # next_multiple_of_4
    padding = "".join("=" for _ in range(next_mult - len(blob)))
    blob += f"{padding}".encode()
    return base64.b64decode(blob)

def encode(blob):
    base64.b64encode(blob).rstrip(b"=")

blob = b"eJxjYBgFo2AUjGQAAAQEAAE"
blob = b"ia"
decoded = decode(blob)
print(decoded)
#len(bin(int.from_bytes(b"ia", "little")).lstrip("0b"))