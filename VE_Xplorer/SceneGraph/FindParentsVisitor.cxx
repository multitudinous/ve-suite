#include "VE_Xplorer/SceneGraph/FindParentsVisitor.h"

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
FindParentsVisitor::FindParentsVisitor( osg::Node* node ):
  NodeVisitor( TRAVERSE_PARENTS )
{
   modelGUID;
   parentNode = 0;
   node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
FindParentsVisitor::~FindParentsVisitor()
{
   parentNode = 0;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* FindParentsVisitor::GetParentNode()
{
   return parentNode.get();
}
////////////////////////////////////////////////////////////////////////////////
void FindParentsVisitor::apply( osg::Node& node )
{ 
   osg::Node::DescriptionList descriptorsList;
   descriptorsList = node.getDescriptions();
   
   //Find the parent node and the id of this particular node
   for ( size_t i = 0; i < descriptorsList.size(); ++i )
   {
      if ( descriptorsList.at( i ) == "VE_XML_ID" )
      {
         modelGUID = descriptorsList.at( i + 1 );
         parentNode = &node;
         break;
      }
   }
   
   //If we did not find an id and therefore a parent then keep going up
   if ( !parentNode )
   {
      osg::NodeVisitor::traverse( node );
   }
}
