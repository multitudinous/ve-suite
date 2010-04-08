// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBBULLET_OSGDB_OSGB__
#define __OSGBBULLET_OSGDB_OSGB__ 1


#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>


class ReaderWriterSGB : public osgDB::ReaderWriter
{
public:
    ReaderWriterSGB();
    ~ReaderWriterSGB();

    const char* className() const;

    virtual osgDB::ReaderWriter::ReadResult readObject( const std::string& fileName, const Options* options=NULL ) const;
    virtual osgDB::ReaderWriter::WriteResult writeObject( const osg::Object& obj, const std::string& fileName, const Options* options=NULL ) const;

protected:
};


// __OSGBBULLET_OSGDB_OSGB__
#endif
