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

#ifndef PROCESSOR_H
#define PROCESSOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/observer_ptr>
#include <osg/Group>

namespace osg
{
class Camera;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace rtt
{

// --- VE-Suite Includes --- //
class Unit;

class VE_SCENEGRAPH_EXPORTS Processor : public osg::Group
{
public:    
    ///Constructor
    Processor();

    ///Copy Constructor
    Processor( const Processor& processor, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, Processor );

    ///Get camera used for this pipeline
    osg::Camera* GetCamera();

    ///Set the camera used for this pipeline
    void SetCamera( osg::Camera* camera );

    ///Mark the underlying unit subgraph as dirty
    void DirtyUnitSubgraph();

    ///Check whenever the subgraph is valid
    bool IsDirtyUnitSubgraph() const;

    ///Force to mark the subgraph as non-dirty
    void MarkUnitSubgraphNonDirty();

    ///Search in the subgraph for a unit
    Unit* FindUnit( const std::string& unitName );

    ///Remove a unit from the processor's subgraph
    bool RemoveUnit( Unit* unit );

    ///Overridden method from osg::Node to allow computation of bounding box
    ///This is needed to prevent traversal of computation down to all children
    ///This method do always returns empty bounding sphere
    osg::BoundingSphere ComputeBound() const;

    ///Set wether or not osg::Clamp should be used in the osgPPU pipelines
    ///This can be a problem when a graphics driver does not support glClamp
    ///By default osg::Clamp will be used
    ///If you do not want osg::Clamp in the pipelines, set to false
    void UseColorClamp( bool useColorClamp = true );

protected:
    ///Destructor
    virtual ~Processor();

private:
    ///
    bool mDirty;

    ///
    bool mDirtyUnitGraph;

    ///
    bool mUseColorClamp;

    ///
    osg::observer_ptr< osg::Camera > mCamera;

};
} //end rtt
} //end scenegraph
} //end xplorer
} //end ves

#endif //PROCESSOR_H
