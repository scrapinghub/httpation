# HTTPation
This is an HTTP request, response, URI and exchange data abstraction and manipulation library.

Requests, responses and URIs are parsed into abstract data types that can then be modified apart
from their serialized representations and serialized back to a binary, UTF-8 string or iodata
form.

An "exchange" is a tuple that wraps a request and its resultant response together with some
metadata about the exchange event.
