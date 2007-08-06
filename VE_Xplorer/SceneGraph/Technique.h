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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef TECHNIQUE_H
#define TECHNIQUE_H

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

// --- OSG Includes --- //
#include <osg/State>
#include <osg/Group>
#include <osg/NodeVisitor>

// --- C/C++ Libraries --- //
#include <vector>
//#include <string>

namespace VE_SceneGraph
{
    class DCS;

class VE_SCENEGRAPH_EXPORTS Technique
{
public:
    Technique();
    virtual ~Technique(){;}

    int GetNumPasses() const;

    osg::StateSet* GetPassStateSet( int i );

    const osg::StateSet* GetPassStateSet( int i ) const;

    virtual void Traverse( osg::NodeVisitor& nv, VE_SceneGraph::DCS* dcs );
        
protected:
    void DirtyPasses();

    void AddPass( osg::StateSet* ss = 0 );

    virtual osg::Node* GetOverrideChild( int );

    //virtual void DefinePasses( VE_SceneGraph::DCS* dcs ) = 0;

    void TraverseImplementation( osg::NodeVisitor& nv, VE_SceneGraph::DCS* dcs );

private:
    std::vector< osg::ref_ptr< osg::StateSet > > m_passes;

};
}

#endif //TECHNIQUE_H
