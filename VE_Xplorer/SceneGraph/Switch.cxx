#include "VE_Xplorer/SceneGraph/Switch.h"

//C/C++ Libraries
#include <iostream>
#include <algorithm>
#include <string>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Switch::Switch()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Switch::Switch(const Switch& switchNode,const osg::CopyOp& copyop):
osg::Switch(switchNode,copyop)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Switch::~Switch()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Switch::SetVal(int whichChildIsActive)
{
#ifdef _PERFORMER
   if ( _switch )
   {
      if(whichChildIsActive == OFF)
         _switch->setVal(-1);
      else
         _switch->setVal(whichChildIsActive);
   }
#elif _OSG
   if(whichChildIsActive == OFF)
   {  
      this->setAllChildrenOff();
   }
   else
   {   
      this->setSingleChildOn(whichChildIsActive);
   }
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::RemoveChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->removeChild( dynamic_cast< Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::AddChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->addChild( dynamic_cast< Node* >( child ));
#endif
   
}
////////////////////////////////////////////////////////////////////////////////
void Switch::InsertChild( int position, SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   this->insertChild( position, dynamic_cast< Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::GetNumChildren( void )
{
#ifdef _OPENSG
   cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->getNumChildren();
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string Switch::GetName( void )
{
#ifdef _OPENSG
   return 0;
#endif
#ifdef _PERFORMER
   return _Switch->getName();
#elif _OSG
   return this->getName().data();
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Switch::SetName( std::string name )
{
#ifdef _OPENSG
   std::cerr << " ERROR: Switch::SetName is NOT implemented " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   _Switch->setName( name.c_str() );
#elif _OSG
   this->setName( name );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OPENSG
   cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool Switch::SearchChild( VE_SceneGraph::SceneNode* searchChild )
{
#ifdef _OPENSG
	
#elif _OSG
	return this->containsNode( dynamic_cast< osg::Node* >(searchChild) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Switch::GetParent( unsigned int position )
{
#ifdef _OPENSG
   
#elif _OSG
	return this->getParent( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Switch::GetChild( unsigned int position )
{
#ifdef _OPENSG
   
#elif _OSG
	return this->getChild( position );
#endif
}
