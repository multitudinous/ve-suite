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

// --- OSG Includes --- //
#include <osg/Transform>
#include <osg/Quat>
#include <osg/Viewport>

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
    enum AutoRotateMode
    {
        NO_ROTATION,
        ROTATE_TO_SCREEN,
        ROTATE_TO_CAMERA
    };

    ///
    ///\param nv
    virtual void accept( osg::NodeVisitor& nv );

    /*
    ///
    virtual AutoTransform* asAutoTransform();

    ///
    virtual const AutoTransform* asAutoTransform() const;
    */

    virtual const char* className() const;

    ///
    ///\return
    virtual osg::Object* cloneType() const;

    ///
    ///\param copyop
    ///\return
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const;

    ///
    ///\return
    virtual osg::BoundingSphere computeBound() const;

    ///
    ///\param matrix
    ///\param nv
    virtual bool computeLocalToWorldMatrix(
        osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    ///
    ///\param matrix
    ///\param nv
    virtual bool computeWorldToLocalMatrix(
        osg::Matrix& matrix, osg::NodeVisitor* nv ) const;

    ///
    ///\return
    AutoRotateMode GetAutoRotateMode() const;

    ///
    bool GetAutoScaleToScreen() const;

    ///
    //double GetAutoScaleTransitionWidthRatio() const;

    ///
    //double GetAutoUpdateEyeMovementTolerance() const;

    ///
    double GetMaximumScale() const;

    ///
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
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    ///\param mode
    void SetAutoRotateMode( AutoRotateMode mode );

    ///
    ///\param autoScaleToScreen
    void SetAutoScaleToScreen( bool autoScaleToScreen );

    ///
    ///\param ratio
    //void SetAutoScaleTransitionWidthRatio( double ratio );

    ///
    ///\param tolerance
    //void SetAutoUpdateEyeMovementTolerance( double tolerance );

    ///
    void SetMaximumScale( double maximumScale );

    ///
    void SetMinimumScale( double minimumScale );

    ///
    void SetPivotPoint( const osg::Vec3d& pivotPoint );

    ///
    void SetPosition( const osg::Vec3d& position );

    ///
    void SetRotation( const osg::Quat& rotation );

    ///
    void SetScale( const double& scale );

    ///
    void SetScale( const osg::Vec3d& scale );

protected:
    ///Destructor
    virtual ~AutoTransform();

private:
    ///
    void computeMatrix() const;

    ///
    void setScale( const double& scale );

    ///
    void setScale( const osg::Vec3d& scale );

    ///
    AutoRotateMode _autoRotateMode;

    ///
    bool _autoScaleToScreen;

    ///
    mutable bool _firstTimeToInitEyePoint;

    ///
    mutable bool _matrixDirty;

    ///
    //double _autoScaleTransitionWidthRatio;

    ///
    //double _autoUpdateEyeMovementTolerance;

    ///
    double _maximumScale;

    ///
    double _minimumScale;

    ///
    //mutable osg::Viewport::value_type _previousWidth;

    ///
    //mutable osg::Viewport::value_type _previousHeight;

    ///
    mutable osg::Vec3 _previousEyePoint;

    ///
    mutable osg::Vec3 _previousLocalUp;

    ///
    osg::Vec3d _pivotPoint;

    ///
    osg::Vec3d _position;

    ///
    mutable osg::Vec3d _previousPosition;

    ///
    mutable osg::Vec3d _scale;

    ///
    osg::Vec3d m_scale;

    ///
    mutable osg::Quat _rotation;

    ///
    mutable osg::Matrixd _cachedMatrix;

    ///
    mutable osg::Matrixd _previousProjection;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_AUTO_TRANSFORM_H
