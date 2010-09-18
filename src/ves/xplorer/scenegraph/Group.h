/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef GROUP_H
#define GROUP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneNode.h>

// --- OSG Includes --- //
#include <osg/Group>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file Group.h
 *
 */

/*!\class ves::xplorer::scenegraph::Group
 *General group node which maintains a list of children
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS Group : public osg::Group, public SceneNode
{
public:
    ///Default constructor
    Group();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Group( const Group&, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, Group );

protected:
    ///Destructor
    virtual ~Group();

public:
    ///Generic set name function
    ///\param name The name of the node
    void SetName( std::string name );

    ///Generic remove child function
    ///\param child The child that is removed
    int RemoveChild( ves::xplorer::scenegraph::SceneNode* child );

    ///Generic add child function
    ///\param child The child that is added
    int AddChild( ves::xplorer::scenegraph::SceneNode* child );

    ///Generic insert child function
    ///\param position The specific location within the group
    ///\param child The child that is inserted
    void InsertChild( int position,
                      ves::xplorer::scenegraph::SceneNode* child );

    ///Generic replace child function
    ///\param childToBeReplaced The child to be replaced
    ///\param newChild The new child
    int ReplaceChild( SceneNode* childToBeReplaced,
                      ves::xplorer::scenegraph::SceneNode* newChild );

    ///Generic search child function
    ///\param searchChild SceneNode* of child to be found
    bool SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild );

    ///Generic find parent function
    ///\param position The position of the parent to be returned
    osg::Group* GetParent( unsigned int position );

    ///Generic get child function
    ///\param position The position of the child to be returned
    osg::Node* GetChild( unsigned int position );

    ///Generic get number of children
    int GetNumChildren();

    ///Get the name of the node
    const std::string GetName();

    ///Set the node mask on or off
    ///\param onOff Node mask state
    void ToggleDisplay( std::string onOff );

    ///Set the node mask on or off
    ///\param onOff Node mask state
    void ToggleDisplay( bool onOff );

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

#endif //GROUP_H
