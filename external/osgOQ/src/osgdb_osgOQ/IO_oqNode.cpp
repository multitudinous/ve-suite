#include "osgOQ/OcclusionQueryNode.h"

#include <iostream>
#include <string>

#include <osg/io_utils>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>
#include <osgDB/ParameterOutput>

bool OQN_readLocalData( osg::Object &obj, osgDB::Input &fr );
bool OQN_writeLocalData( const osg::Object &obj, osgDB::Output &fw );

osgDB::RegisterDotOsgWrapperProxy OcclusionQueryNode_Proxy
(
    new osgOQ::OcclusionQueryNode,
    "OcclusionQueryNode",
    "Object Node OcclusionQueryNode Switch Group",
    OQN_readLocalData,
    OQN_writeLocalData
);

bool OQN_readLocalData( osg::Object &obj, osgDB::Input &fr )
{
    osgOQ::OcclusionQueryNode& oqn = static_cast<osgOQ::OcclusionQueryNode&>( obj );
    bool advanced( false );

    //osgOQ::OcclusionQueryContext* oqc = fr.readDrawable();
    //oqn.setOQC( oqc );

	if (fr[0].matchWord( "QueriesEnabled" ))
	{
		bool enable( fr[1].getStr() == "TRUE" );
		oqn.setQueriesEnabled( enable );
        fr+=2;
		advanced = true;
    }

    return advanced;
}

bool OQN_writeLocalData( const osg::Object &obj, osgDB::Output &fw )
{
    const osgOQ::OcclusionQueryNode& oqn = static_cast<const osgOQ::OcclusionQueryNode&>( obj );

    //fw.writeObject( oqn.getOQN(i));

    fw.indent() << "QueriesEnabled " <<
		(oqn.getQueriesEnabled() ? "TRUE" : "FALSE")
		<< std::endl;

    return true;
}
