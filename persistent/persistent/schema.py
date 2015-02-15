from sqlalchemy import Column, ForeignKey, Integer, String, DateTime
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship

from persistent import engine

import datetime

Base = declarative_base()

class Term(Base):
    __tablename__ = 'term'
    # Here we define columns for the table person
    # Notice that each column is also a normal Python instance attribute.
    id = Column(Integer, primary_key=True)
    name = Column(String(250), nullable=False)

class Entry(Base):
    __tablename__ = 'Entry'
    # Here we define columns for the table address.
    # Notice that each column is also a normal Python instance attribute.
    id = Column(Integer, primary_key=True)
    author = Column(String(250))
    text = Column(String(512))
    date_added = Column(DateTime, default=datetime.datetime.utcnow)
    term = relationship(Term)
    term_id = Column(Integer, ForeignKey('term.id'))

# Create all tables in the engine. This is equivalent to "Create Table"
# statements in raw SQL.
Base.metadata.create_all(engine)
