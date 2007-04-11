#include "VE_Xplorer/SceneGraph/UpdateIDOnChildrenVisitor.h"

#include "VE_xplorer/SceneGraph/DCS.h"

#include <osg/Node>

using namespace VE_SceneGraph;

#include <iostream>

////////////////////////////////////////////////////////////////////////////////
UpdateIDOnChildrenVisitor::UpdateIDOnChildrenVisitor( VE_SceneGraph::DCS* node, std::string ID ):
  NodeVisitor( TRAVERSE_ALL_CHILDREN ),
  modelGUID( ID )
{
   node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
UpdateIDOnChildrenVisitor::~UpdateIDOnChildrenVisitor()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void UpdateIDOnChildrenVisitor::apply( osg::PositionAttitudeTransform& node )
{
   osg::Node::DescriptionList descriptorsList;
   descriptorsList.push_back( "VE_XML_ID" );
   descriptorsList.push_back( modelGUID );
   
   node.setDescriptions( descriptorsList );
   
   osg::NodeVisitor::traverse( node );
}
