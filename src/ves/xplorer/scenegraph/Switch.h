/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2009 by Iowa State University
*
* Original Development Team:
*   - ISU's Thermal Systems Virtual Engineering Group,
*     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
*   - Reaction Engineering International, www.reaction-eng.com
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the
* Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* Boston, MA 02111-1307, USA.
*
* -----------------------------------------------------------------
* Date modified: $Date$
* Version:       $Rev$
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef SWITCH_H
#define SWITCH_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneNode.h>

// ---  OSG Includes --- //
#include <osg/Switch>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file Switch.h
 *
 */

/*!\class ves::xplorer::scenegraph::Switch
 *
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS Switch : public osg::Switch, public SceneNode
{
public:
    ///Base Constructor
    Switch();

    ///Copy constructor using CopyOp to manage deep vs shallow copy.
    Switch( const Switch&, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, Switch );

protected:
    ///Destructor
    virtual ~Switch();

public:
    ///
    enum Value
    {
        OFF = -1, ON
    };

    //probably need more functions but
    //this is all we need for now
    //the rest are inherited from group
    void SetVal( int whichChildIsActive );

    ///Generic set name function
    ///\param name The name
    void SetName( std::string name );

    ///Generic remove child function
    ///\param child The child to be removed
    int RemoveChild( SceneNode* child );

    ///Generic add child function
    ///\param The child to be added
    int AddChild( SceneNode* child );

    ///Generic insert child function
    ///\param position The position of the child to be inserted
    ///\param child The child to be inserted
    void InsertChild( int position, SceneNode* child );

    ///Generic replace child function
    ///\param childToBeReplaced The child to be replaced
    ///\param newChild The new child
    int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );

    ///Generic search child function
    ///\param searchChild SceneNode* of child to be found
    bool SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild );

    ///Generic find parent function
    ///\param position the position of the parent to be returned
    osg::Group* GetParent( unsigned int position );

    ///Generic get child function
    ///\param position the position of the child to be returned
    osg::Node* GetChild( unsigned int position );

    ///Generic get number of children
    int GetNumChildren();

    ///Set the name of the node
    const std::string GetName();

// -------------------------------------------------- //
// --- This stuff is used for multipass rendering --- //
// -------------------------------------------------- //
public:
    ///
    virtual void traverse( osg::NodeVisitor& nv );

    ///
    virtual void InheritedTraverse( osg::NodeVisitor& nv );

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //SWITCH_H
