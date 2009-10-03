// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_OSGDB_OSGB__
#define __OSGBULLET_OSGDB_OSGB__ 1


#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>


class ReaderWriterOSGB : public osgDB::ReaderWriter
{
public:
    ReaderWriterOSGB();
    ~ReaderWriterOSGB();

    const char* className() const;

    virtual osgDB::ReaderWriter::ReadResult readObject( const std::string& fileName, const Options* options=NULL ) const;
    virtual osgDB::ReaderWriter::WriteResult writeObject( const osg::Object& obj, const std::string& fileName, const Options* options=NULL ) const;

protected:
};


// __OSGBULLET_OSGDB_OSGB__
#endif
