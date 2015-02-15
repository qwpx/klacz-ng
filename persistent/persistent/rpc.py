from papoprot.server import RPCServer, rpc_method
from txzmq import ZmqEndpoint, ZmqFactory, ZmqREPConnection
from twisted.internet import reactor
from optparse import OptionParser
import sqlalchemy

from persistent import engine, DBSession
from persistent import persistent_pb2
from persistent.persistent_pb2 import CreateTermRequest, CreateTermResponse
from persistent.persistent_pb2 import GetTermRequest, GetTermResponse
from persistent.persistent_pb2 import CreateEntryRequest, CreateEntryResponse
from persistent.persistent_pb2 import GetEntriesRequest, GetEntriesResponse
from persistent.schema import *

class PersistentServer(RPCServer):
    def GetTermByName(self, session, name):
        return session.query(Term).filter(Term.name == name).one()

    @rpc_method(CreateTermRequest, CreateTermResponse)
    def CreateTerm(self, request):
        if not request.HasField("name"):
            raise RPCError("CreateTerm request requires term name.")
        session = DBSession()
        new_term = Term(name=request.name)
        session.add(new_term)
        session.commit()
        return CreateTermResponse(term=persistent_pb2.Term(
                id=new_term.id, name=new_term.name))

    @rpc_method(GetTermRequest, GetTermResponse)
    def GetTerm(self, request):
        if not request.HasField("name"):
            raise RPCError("GetTerm requires term name.")

        response = GetTermResponse()
        session = DBSession()
        try:
            term = self.GetTermByName(session, request.name)
            response.term.id=term.id
            response.term.name=term.name
        except sqlalchemy.orm.exc.NoResultFound:
            pass
        return response

    @rpc_method(CreateEntryRequest, CreateEntryResponse)
    def CreateEntry(self, request):
        if not request.HasField("term"):
            raise RPCError("CreateEntry request requires term.")
        if not request.term.HasField("name"):
            raise RPCError("CreateEntry request's term requires name.")
        if not request.HasField("author"):
            raise RPCError("CreateEntry request requires author.")
        if not request.HasField("text"):
            raise RPCError("CreateEntry request requires text.")

        session = DBSession()
        try:
            term = self.GetTermByName(session, request.term.name)
        except sqlalchemy.orm.exc.NoResultFound:
            raise RPCError("no such term")
        entry = Entry(author=request.author, text=request.text, term=term)
        session.add(entry)
        session.commit()
        return CreateEntryResponse(entry=persistent_pb2.Entry(
            id=entry.id,
            author=entry.author,
            text=entry.text,
            date_added=str(entry.date_added),
            term_id=term.id))

    @rpc_method(GetEntriesRequest, GetEntriesResponse)
    def GetEntries(self, request):
        if not request.HasField("term"):
            raise RPCError("CreateEntry request requires term.")
        if not request.term.HasField("name"):
            raise RPCError("CreateEntry request's term requires name.")

        session = DBSession()
        try:
            term = self.GetTermByName(session, request.term.name)
        except sqlalchemy.orm.exc.NoResultFound:
            raise RPCError("no such term")
        entries = session.query(Entry).filter(Entry.term_id == term.id).all()
        response = GetEntriesResponse()
        for entry in entries:
            pb_entry = response.entries.add()
            pb_entry.id = entry.id
            pb_entry.author = entry.author
            pb_entry.text = entry.text
            pb_entry.date_added = str(entry.date_added)
            pb_entry.term_id = entry.term_id
        return response

def MakePersistentServer(endpoint):
    zf = ZmqFactory()
    e = ZmqEndpoint("bind", endpoint)
    return PersistentServer(zf, e)

if __name__ == "__main__":
    parser = OptionParser("")
    parser.add_option("-e", "--endpoint", dest="endpoint", help="0MQ Endpoint")
    (options, args) = parser.parse_args()

    s = MakePersistentServer(options.endpoint)
    reactor.run()
