/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
* Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
* Version:       $Rev: 8206 $
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SET_STATE_ON_NURBS_NODE_VISITOR_H
#define SET_STATE_ON_NURBS_NODE_VISITOR_H

/*!\file SetStateOnNURBSVisitor.h
*/

/*!\class VE_SceneGraph::SetStateOnNURBSVisitor
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>

// --- C/C++ Libraries
#include <string>
#include <utility>

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS SetStateOnNURBSNodeVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to be traversed
    ///\param selectedState The state of the selection process
    ///\param dragState The state of the mouse true for dragging
    ///\param mousePoint The mouse position is between 0,1
    SetStateOnNURBSNodeVisitor( osg::Node* node, bool selectedState, 
        bool dragState,
        std::pair< double, double > mousePoint, 
        std::pair< double, double > mouseDelta );

    ///Destructor
    virtual ~SetStateOnNURBSNodeVisitor( void );

    ///Apply function that gets called during the traversal
    ///\param node A child osg::PositionAttitudeTransform w/in the traversed node
    virtual void apply( osg::Node& node );

private:
    ///Flag to set the selected state
    bool m_selectedState;
    ///The drag state of the mouse
    bool m_dragState;
    ///Mouse location
    std::pair< double, double > m_mousePoint;
    ///Delta for the mouse to move
    std::pair< double, double > m_mouseDelta;
};
}

#endif //SET_STATE_ON_NURBS_NODE_VISITOR_H
