#ifndef VE_SWITCH_H
#define VE_SWITCH_H
/*!\file Switch.h
Switch API
*/

/*!\class VE_SceneGraph::Switch
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfSwitch.h>
#elif _OSG
#include <osg/Switch>
#elif OPENSG
#endif

namespace VE_SceneGraph{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS Switch : public osg::Switch, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS Switch : public pfSwitch, public SceneNode
#endif
{
public:
   Switch();
   ///Copy constructor
   /// Copy constructor using CopyOp to manage deep vs shallow copy.
   Switch( const Switch&, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );
   
   META_Node( VE_SceneGraph, Switch );

protected:
   virtual ~Switch();
   
public:
   enum Value{OFF=-1,ON};
   //probably need more functions but
   //this is all we need for now
   //the rest are inherited from group
   void SetVal(int whichChildIsActive);
   ///Generic set name function
   ///\param
   void SetName( std::string name );
   ///Generic remove child function
	///\param
   int RemoveChild( SceneNode* child );
   ///Generic add child function
	///\param
   int AddChild( SceneNode* child );
   ///Generic insert child function
	///\param
   void InsertChild( int position, SceneNode* child );
   ///Generic replace child function
	///\param
   int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );
	///Generic search child function
	///\param searchChild SceneNode* of child to be found
	bool SearchChild( VE_SceneGraph::SceneNode* searchChild );
	///Generic find parent function
	///\param position the position of the parent to be returned
	osg::Group* GetParent( unsigned int position );
	///Generic get child function
	///\param position the position of the child to be returned
	osg::Node* GetChild( unsigned int position );
   ///Generic get number of children
   int GetNumChildren( void );
   ///Set the name of the node
   const std::string GetName( void );
};
}

#endif// VE_SWITCH_H
