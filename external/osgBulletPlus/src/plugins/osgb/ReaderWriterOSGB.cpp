// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include "ReaderWriterOSGB.h"

#include "osgbBullet/PhysicsState.h"
#include "osgbBullet/OSGToCollada.h"
#include "osgwTools/RefID.h"

#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>
#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

#include <iostream>
#include <string>


ReaderWriterOSGB::ReaderWriterOSGB()
{
    supportsExtension( "osgb", "osgbBullet physics simulation state." );

    supportsOption( "TBD", "To Be Determined." );
}
ReaderWriterOSGB::~ReaderWriterOSGB()
{
}

const char*
ReaderWriterOSGB::className() const
{
    return "osgbBullet physics simulation state";
}

osgDB::ReaderWriter::ReadResult
ReaderWriterOSGB::readObject( const std::string& fileName, const Options* options ) const
{
    const std::string ext = osgDB::getFileExtension( fileName );
    if( !acceptsExtension( ext ) )
    {
        osg::notify( osg::INFO ) << "OSGB: Unsupported extension " << fileName << std::endl;
        return( osgDB::ReaderWriter::ReadResult::FILE_NOT_HANDLED );
    }

    const std::string fullName = osgDB::findDataFile( fileName );
    if( fullName.empty() )
        return( osgDB::ReaderWriter::ReadResult::FILE_NOT_FOUND );

    std::ifstream ifs( fullName.c_str() );
    if( !ifs.good() )
        return( osgDB::ReaderWriter::ReadResult::ERROR_IN_READING_FILE );

    osgDB::Input fr;
    fr.attach( &ifs );


    unsigned int numEntries( 0 );
    if( fr.matchSequence( "Physics data entries %i" ) )
    {
        fr[3].getUInt( numEntries );
        fr+=4;
    }
    if( numEntries == 0 )
        return( osgDB::ReaderWriter::ReadResult::ERROR_IN_READING_FILE );
    osg::notify( osg::INFO ) << "OSGB: " << numEntries << " entries." << std::endl;

    osg::ref_ptr< osgbBullet::PhysicsState > ps = new osgbBullet::PhysicsState;
    unsigned int idx;
    for( idx = 0; idx < numEntries; idx++ )
    {
        osg::notify( osg::INFO ) << "OSGB: Reading entry " << idx << std::endl;

        osg::ref_ptr< osgwTools::RefID > rid = static_cast< osgwTools::RefID* >( fr.readObject() );
        if( rid == NULL )
        {
            osg::notify( osg::INFO ) << "OSGB: Failed, rid " << rid.get() << std::endl;
            return( osgDB::ReaderWriter::ReadResult::ERROR_IN_READING_FILE );
        }
        osg::ref_ptr< osgbBullet::PhysicsData > pd = static_cast< osgbBullet::PhysicsData* >( fr.readObject() );
        if( pd == NULL )
        {
            osg::notify( osg::INFO ) << "OSGB: Failed, pd " << pd.get() << std::endl;
            return( osgDB::ReaderWriter::ReadResult::ERROR_IN_READING_FILE );
        }
        ps->addPhysicsData( rid.get(), pd.get() );

        osg::notify( osg::INFO ) << "OSGB: Finished reading entry " << idx << std::endl;
    }


    return( ps.release() );
}

osgDB::ReaderWriter::WriteResult
ReaderWriterOSGB::writeObject( const osg::Object& obj, const std::string& fileName, const Options* options ) const
{
    osgDB::Output fw( fileName.c_str() );
    if( !fw.good() )
        return( osgDB::ReaderWriter::WriteResult::ERROR_IN_WRITING_FILE );

    const osgbBullet::PhysicsState* ps = dynamic_cast< const osgbBullet::PhysicsState* > ( &obj );
    if( ps == NULL )
        return( osgDB::ReaderWriter::WriteResult::ERROR_IN_WRITING_FILE );


    const osgbBullet::PhysicsState::DataMap& dm = ps->getDataMap();
    fw << "Physics data entries " << dm.size() << std::endl;

    osgbBullet::PhysicsState::DataMap::const_iterator it;
    for( it = dm.begin(); it != dm.end(); it++ )
    {
        osg::ref_ptr< osgwTools::RefID > rid = new osgwTools::RefID( it->first );
        fw.writeObject( *rid );
        fw.writeObject( *(it->second) );
    }

    return( osgDB::ReaderWriter::WriteResult::FILE_SAVED );
}


REGISTER_OSGPLUGIN( osgb, ReaderWriterOSGB )
