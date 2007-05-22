#include "osgOQ/OcclusionQueryRoot.h"

#include <iostream>
#include <string>

#include <osg/io_utils>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>
#include <osgDB/ParameterOutput>

bool OQR_readLocalData( osg::Object &obj, osgDB::Input &fr );
bool OQR_writeLocalData( const osg::Object &obj, osgDB::Output &fw );

osgDB::RegisterDotOsgWrapperProxy OcclusionQueryRoot_Proxy
(
    new osgOQ::OcclusionQueryRoot,
    "OcclusionQueryRoot",
    "Object Node OcclusionQueryRoot Group",
    OQR_readLocalData,
    OQR_writeLocalData
);

bool OQR_readLocalData( osg::Object &obj, osgDB::Input &fr )
{
    osgOQ::OcclusionQueryRoot& oqr = static_cast<osgOQ::OcclusionQueryRoot&>( obj );
    bool advanced( false );

    //osgOQ::OcclusionQueryContext* oqc = fr.readDrawable();
    //oqn.setOQC( oqc );

    if (fr[0].matchWord( "QueriesEnabled" ))
	{
		bool enable( fr[1].getStr() == "TRUE" );
		oqr.setQueriesEnabled( enable );
        fr+=2;
		advanced = true;
    }

    return advanced;
}

bool OQR_writeLocalData( const osg::Object &obj, osgDB::Output &fw )
{
    const osgOQ::OcclusionQueryRoot& oqr = static_cast<const osgOQ::OcclusionQueryRoot&>( obj );

    //fw.writeObject( oqn.getOQN(i));

    fw.indent() << "QueriesEnabled " <<
		(oqr.getQueriesEnabled() ? "TRUE" : "FALSE")
		<< std::endl;

	return true;
}

