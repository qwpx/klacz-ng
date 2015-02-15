from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

engine = create_engine('sqlite:///sqlalchemy_example.db')
DBSession = sessionmaker(bind=engine)
