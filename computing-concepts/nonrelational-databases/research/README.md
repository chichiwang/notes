# Research: Non-Relational Databases
A high level investigation into non-relational databases.

Notes ported over from Google Docs, built between May 20, 2018 and June 2, 2018.

## Table of Contents
* [Characteristics of NoSQL](#characteristics-of-nosql)
* [Types of NoSQL Databases](#types-of-nosql-databases)
  * [Column Store/Distributed Peer Store](#column-storedistributed-peer-store)
  * [Document Store](#document-store)
  * [Key/Value Store](#keyvalue-store)
  * [Graph Store](#graph-store)
  * [Object Store](#object-store)
  * [XML Store](#xml-store)
* [Examples of NoSQL DBMSs](#examples-of-nosql-dbmss)
* [Advantages](#advantages)
* [Disadvantages](#disadvantages)
* [Why Choose NoSQL?](#why-choose-nosql)
* [History](#history)
* [Resources](#resources)

## Characteristics of NoSQL
Relational databases have a fixed table structure. NoSQL databases are a form of unstructured storage.

> Not always, according to a dissenting comment: See below.

## Types of NoSQL Databases
### Column Store/Distributed Peer Store
* Data is stored in columns
  * As opposed to rows in relational databases
* Comprised of one or more Column Families
  * Logically group certain columns in the DB
* Key is used to point to a number of columns in the DB
  * Keyspace attribute defines the scope of the key
* Columns contain tuples of names and values
  * Ordered and comma-separated

**Pros**
* Fast read/write access to the data stored
* Rows that correspond to a column are stored as a single disk entry

**Cons**
* Very low level API

**Applications**: Google BigTable, HBase, Cassandra, Riak

### Document Store
* Schema-less
* Similar to a Key/Value store
* Aggregate-oriented
  * Groupings of data that can be retrieved wholesale
  * Able to save whole aggregates to disk
  * A document is the aggregate
* Values provide encoding for the data stored
  * XML, JSON, BSON (Binary encoded JSON)
* Querying based on data is possible

**Pros**
* Tolerant of incomplete data

**Cons**
* Lack of consistency at the DB level
* Query performance
* No standard query syntax

**Applications**: MongoDB, CouchDB

### Key/Value Store
* Hash table
* Schema-less
* Aggregate-oriented
  * Groupings of data that can be retrieved wholesale
  * Able to save whole aggregates to disk
  * A value is the aggregate
* Some implementations provide caching mechanisms
* Data stored in the form of string, JSON, or BLOB (Binary large object)

**Pros**
* Fast lookups

**Cons**
* Lack of consistency at the DB level
* Stored data has no schema

**Applications**: Amazon DynamoDB, Redis, Oracle BDB, Voldemort, Tokyo Cabinet/Tyrant

### Graph Store
* Directed graph structure represents the data
* Comprised of *Edges* and *Nodes*
  * Mathematically *Nodes* are *Vertices* and *Edges* are *Links* between vertices
* Nodes are organized by some relationship with each other
* Nodes and Edges have defined properties
* Typically used in social networking applications
* Relationships are first class citizens

**Pros**
* Allows developers to focus more on the relationships between objects rather than the objects themselves
  * JOINs all day long
* Graph algorithms (eg: shortest path)
* Connectedness
* N-degree relationships

**Cons**
* Has to traverse an entire graph to reach a definitive answer
* Not easy to cluster

**Applications**: InfoGrid, InfiniteGraph, Neo4J

### Object Store
**Pros**
* Matches OO development paradigm
* Low-latency ACID (atomicity, consistency, isolation, durability)

**Cons**
* Limited querying/batch-update options

**Applications**: Oracle Coherence, db4o, Object Store, Gemstone, Polar

### XML Store
**Pros**
* Mature search technologies
* Schema validation

**Cons**
* No real binary solution
* Easier to rewrite documents than to update them

**Applications**: Exist, Oracle, MarkLogic

## Examples of NoSQL DBMSs
* Google BigTable
* Amazon DynamoDB
* Hypertable
* Cassandra
* Accumulo
* HyperTable
* HBase
* MongoDB
* CouchDB
* Mark Logic
* DynamoDB
* Rides
* Riak
* HBase
* Redis
* Neo4J
* InfiniteGraph

## Advantages
* Simple, flexible structure
  * *Debated in article comments: Neo4J has a fixed structure. HBase and Google BigTable are tabular.*
* Schema-free
  * *Debated in article comments: does not apply to all NoSQL DBs. Cassandra may use schemas.*
* Open-source DBMS options
  * Don’t require licensing fees
  * Can run on inexpensive hardware
  * Cost-effective deployments
* Expansion is easier/cheaper
  * Utilize horizontal scaling or load distribution on all nodes
    * *Debated in article comments: commenter finds MySQL/Percona DBs easier to scale horizontally than Redis. Disagrees that SQL DBs need to be vertically scaled.*
* Offers higher performance, high scalability, and ease-of-access
  * Especially in applications where massive data is stored and processed frequently
* Improved data comprehension
  * Depending on type of storage/retrieval actually performed by the org/app
  * Many NoSQL DBs are designed to run on clusters, instead of a single machine like relational DBs are
* Application development efficiency
  * Provides a data model that better fits an application’s needs

## Disadvantages
* Don’t support reliability features that come with relational databases
  * Atomicity, consistency, isolation, durability
  * Trade consistency for performance and scalability
  * Developers must implement consistency features, which adds complexity to the system
  * **Note**: Relational DBs also have consistency issues in concurrent operations, transactions don’t solve all of these problems
* Don’t support SQL queries
  * Manual querying or proprietary querying language is needed
    * Adds time and complexity
* Lack features that provide consistency and reliability
  * A number of NoSQL DBMSs offer new features to enhance scalability and reliability
* Specialized solution
  * The number of applications that can rely on NoDQL DBs is still limited
* *Added in the comments: NoSQL DBs are not relational*
  * *Understanding when to use them is key*
* Can lack full-featured syntax
  * *“...people who produce complex SQL queries are likely to balk at being asked to replicate operators like SUM, ORDER BY and GROUP in a map-reduce job that they have to create themselves in Javascript”*
* Must pay a cost when you want to look at the data in a way the aggregates were not originally designed (except Graph DBs)

## Why choose NoSQL?
* A relational db will not scale to your use case at an acceptable cost
* Your entity-relationship-diagram can not be printed on an A3 sheet of paper
  * You may be storing too much in a single database
* Your business model generates a lot of temporary data that does not belong in the main data store
  * Shopping cart
  * Retained searches
  * Site personalization
  * Incomplete questionnaires
  * Cache data
* Your relational db has been normalized for performance or convenience in manipulating data
* Your dataset consists of large quantities of text/images and column definitions are large object (CLOB/BLOB)
* You need to run queries that do not involve simple hierarchical relations
  * Recommendations/business intelligence questions that involve an absence of data
    * "all people in a social network who have not purchased a book this year who are once removed from people who have"
* You have local data transactions that do not have to be durable
  * Liking items on websites
    * “creating transactions for these kind of interactions are overkill because if the action fails the user is likely to just repeat it until it works”
  * Ajax-heavy sites tend to have a lot of these use-cases
* It would be advantageous to migrate data structures without having to take the data store offline

## History
**1980s**: Rise Of Relational Databases
* Relational DB strengths: Persistence, integration, SQL, transactions, reporting
* Relational DB weakness: structure of DB is different than structure of data in application memory (impedance mismatch)

**1990s**: Object Databases Introduced
* Did not gain much steam in practice
* May have failed to gain traction because of the way businesses integrate data (integration model) - many applications reading from the same DB

**2000s**: Relational DB Dominance
* Relational DBs dominate the industry landscape
* Start to see limitations of relational DBS
  * Rise in massive traffic to certain sites
  * Scaling became difficult: scaling DBs means scaling machines and relational DBs were not designed with this scaling pattern in mind
  * New solutions are designed
    * Google develops BigTable
    * Amazon develops DynamoDB
    * Closed, proprietary systems to start
    * Addresses problems of high traffic and needs of lots and lots of nodes
  * Name “NoSQL” is coined via a hashtag for a meetup
    * Characteristics of NoSQL DBs: non-relational, open source, cluster friendly, 21st century web, schema-less

**2010s**: And Beyond
* DBMS options: SQL, Aggregate, Graph

## Resources
* [The Definitive Guide to NoSQL Databases](https://www.toptal.com/database/the-definitive-guide-to-nosql-databases)
* [NoSQL Introduction and different NoSQL Database](http://programmershelper.com/nosql/)
* [NoSQL, No Problem: An Intro To NoSQL Databases](https://www.thoughtworks.com/insights/blog/nosql-no-problem-intro-nosql-databases)
* [NoSQL Distilled *](http://bigdata-ir.com/wp-content/uploads/2017/04/NoSQL-Distilled.pdf)
* [NoSQL Distilled to an Hour](https://www.youtube.com/watch?v=ASiU89Gl0F0)
