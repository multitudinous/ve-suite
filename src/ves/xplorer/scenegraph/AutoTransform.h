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

#ifndef VES_XPLORER_SCENEGRAPH_AUTO_TRANSFORM_H
#define VES_XPLORER_SCENEGRAPH_AUTO_TRANSFORM_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

// --- OSG Includes --- //
#include <osg/Transform>
#include <osg/Quat>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
/*!\file AutoTransform.h
 * AutoTransform API
 */

/*!\class ves::xplorer::scenegraph::AutoTransform
 *
 */
class VE_SCENEGRAPH_EXPORTS AutoTransform : public osg::Transform
{
public:
    ///
    AutoTransform();

    ///
    ///\param autoTransform
    ///\param copyop
    AutoTransform(
        const AutoTransform& autoTransform,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, AutoTransform );

    ///
    enum AutoRotateMode
    {
        NO_ROTATION,
        ROTATE_TO_SCREEN,
        ROTATE_TO_CAMERA
    };

    ///
    ///\return
    virtual osg::BoundingSphere computeBound() const;

    ///
    ///\param matrix
    ///\param nv
    ///\return
    virtual bool computeLocalToWorldMatrix(
        osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    ///
    ///\param matrix
    ///\param nv
    ///\return
    virtual bool computeWorldToLocalMatrix(
        osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    ///
    ///\return
    AutoRotateMode GetAutoRotateMode() const;

    ///
    ///\return
    bool GetAutoScaleToScreen() const;

    ///
    //scenegraph::GLTransformInfoPtr GetCurrentGLTransformInfo() const;

    ///
    ///\return
    double GetMaximumScale() const;

    ///
    ///\return
    double GetMinimumScale() const;

    ///
    ///\return
    const osg::Vec3d& GetPivotPoint() const;

    ///
    ///\return
    const osg::Vec3d& GetPosition() const;

    ///
    ///\return
    const osg::Vec3d GetPreviousEyePoint() const;

    ///
    ///\return
    const osg::Vec3d GetPreviousLocalUp() const;

    ///
    ///\return
    const osg::Quat& GetRotation() const;

    ///
    ///\return
    const osg::Vec3d& GetScale() const;

    ///
    ///\param mode
    void SetAutoRotateMode( AutoRotateMode mode );

    ///
    ///\param autoScaleToScreen
    void SetAutoScaleToScreen( bool autoScaleToScreen );

    ///
    ///\param
    virtual void SetCurrentGLTransformInfo(
        GLTransformInfoPtr currentGLTransformInfo );

    ///
    ///\param maximumScale
    void SetMaximumScale( double maximumScale );

    ///
    ///\param minimumScale
    void SetMinimumScale( double minimumScale );

    ///
    ///\param pivotPoint
    void SetPivotPoint( const osg::Vec3d& pivotPoint );

    ///
    ///\param position
    void SetPosition( const osg::Vec3d& position );

    ///
    ///\param rotation
    void SetRotation( const osg::Quat& rotation );

    ///
    ///\param scale
    void SetScale( const double& scale );

    ///
    ///\param scale
    void SetScale( const osg::Vec3d& scale );

protected:
    ///Destructor
    virtual ~AutoTransform();

    ///
    GLTransformInfoPtr m_currentGLTransformInfo;

private:
    ///
    ///\param scale
    void setScale( const double& scale );

    ///
    ///\param scale
    void setScale( const osg::Vec3d& scale );

    ///
    AutoRotateMode _autoRotateMode;

    ///
    bool _autoScaleToScreen;

    ///
    mutable bool _firstTimeToInitEyePoint;

    /// 
    //mutable bool _matrixDirty;

    ///
    double _maximumScale;

    ///
    double _minimumScale;

    ///
    mutable osg::Vec3 _previousEyePoint;

    ///
    mutable osg::Vec3 _previousLocalUp;

    ///
    osg::Vec3d _pivotPoint;

    ///
    osg::Vec3d _position;

    ///
    mutable osg::Vec3d _scale;

    ///
    osg::Vec3d m_scale;

    ///
    mutable osg::Quat _rotation;

    ///
    //mutable osg::Matrixd _cachedMatrix;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_AUTO_TRANSFORM_H
