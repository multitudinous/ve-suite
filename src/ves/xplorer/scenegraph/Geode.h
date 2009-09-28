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

#ifndef GEODE_H
#define GEODE_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneNode.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Geode>

// --- VTK Includes --- //
class vtkActor;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file Geode.h
 *
 */

/*!\class ves::xplorer::scenegraph::Geode
 *A leaf node on the scene graph
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS Geode : public osg::Geode, public SceneNode
{
public:
    ///Constructor
    Geode();

protected:
    ///Destructor
    virtual ~Geode();

public:
    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ///\param geode
    ///\param copyop
    Geode( const Geode& geode, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, Geode );

    ///Turn vtkActorToXX on and off
    ///\param onOff The VTK debug level
    void TurnOnDebugOutput( int onOff = 0 )
    {
        _vtkDebugLevel = onOff;
    }

    ///This function implements the respective translate vtkActorToGeode
    ///\param actor The VTK actor to be translated to a geode
    void TranslateToGeode( vtkActor* actor );

    void StreamLineToGeode( vtkActor* actor );

    ///Generic get parent function
    ///\param position The position of the parent to be returned
    osg::Group* GetParent( unsigned int position );

protected:
    ///The VTK debug level
    int _vtkDebugLevel;

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

#endif //GEODE_H
