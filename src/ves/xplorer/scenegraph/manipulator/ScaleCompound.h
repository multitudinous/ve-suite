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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_SCALE_COMPOUND_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_SCALE_COMPOUND_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/CompoundDragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class ScaleAxis;
class ScaleUniform;

/*!\file ScaleCompound.h
 * ScaleCompound API
 */

/*!\class ves::xplorer::scenegraph::ScaleCompound
 *
 */
class VE_SCENEGRAPH_EXPORTS ScaleCompound : public CompoundDragger
{
public:
    ///Constructor
    ScaleCompound();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ScaleCompound(
        const ScaleCompound& scaleCompound,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\return
    virtual const char* className() const;

    ///
    ///\param copyop
    ///\return
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const;

    ///
    ///\return
    virtual osg::Object* cloneType() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

protected:
    ///Destructor
    virtual ~ScaleCompound();

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    const double m_explodeDistance;

    ///
    osg::ref_ptr< ScaleAxis > m_xScaleAxis;
    
    ///
    osg::ref_ptr< ScaleAxis > m_yScaleAxis;
    
    ///
    osg::ref_ptr< ScaleAxis > m_zScaleAxis;

    ///
    osg::ref_ptr< ScaleUniform > m_scaleUniform;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_SCALE_COMPOUND_H
