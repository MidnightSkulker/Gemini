# Gemini
# Code demo for Gemini

Implementation Notes

1. The web server is implemented in Haskell using the Scotty web Server
framework. This is a monad (inevitably). I also used an extension
called WebM, which I extended a little bit. The makes a stack of monads
including Scotty, WebM, STM, and IO. STM is Software Transactional Memory,
which supports atomic access to the global data store.

Global Data Store
I chose to implement this as Haskell Data Structures, instead of putting
a DB underneath the web server. This permitted me to show some coding
skills around data structures. This includes using automatically derived
instances of ToJSON, allowing me to serialize a data structure for output.
HOWEVER: Sometimes a single web call involves more than one atomic access
or update to the global data. This permits a time window between the two
atomic operations where another thread or process could introduce a change
to the global data, making the view on the second update inconsistent with
the view on the first call. I am going to work on solving this by
providing more sophisticated operations down at the STM layer monad,
which can then all be done together atomically, providing a inconsistent
view of the data across this short sequence of updates and accesses. I am
submitting the coding challenge now without this update, because I think
what I have so far is worth looking at.

Security Vulnerabilities
The specification appears to say that job coin addresses are *any*
sequence of characters. The input fields in the web pages therefore allow
the user to input arbitrary strings, including script. This is definitely
a threat for XSS, or SQL injection if a database were under the web server.
This is a fix that is needed, but requires discussion with the potential
customers to make sure that whatever restrictions are enforced meet their
needs.
