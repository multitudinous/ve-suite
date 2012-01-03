/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_PAN_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_PAN_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file TranslatePan.h
 * TranslatePan API
 */

/*!\class ves::xplorer::scenegraph::TranslatePan
 *
 */
class VE_SCENEGRAPH_EXPORTS TranslatePan : public Dragger
{
public:
    ///
    TranslatePan();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    TranslatePan(
        const TranslatePan& translatePan,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    virtual TranslatePan* AsTranslatePan();

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

protected:
    ///
    virtual ~TranslatePan();

    ///
    virtual void ComputeDeltaTransform();

    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint );

    ///
    virtual void SetupDefaultGeometry();

private:


};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_PAN_H
