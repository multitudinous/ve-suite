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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

// --- OSG Includes --- //
#include <osg/AutoTransform>

#include <osgUtil/CullVisitor>
#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger(
    const AxesFlag::Enum& axesFlag,
    const TransformationType::Enum& transformationType,
    Manipulator* const parentManipulator )
    :
    osg::MatrixTransform(),
    m_axesFlag( axesFlag ),
    m_transformationType( transformationType ),
    m_comboForm( false ),
    m_color( NULL ),
    m_parentManipulator( parentManipulator )
{
    m_colorMap[ ColorTag::DEFAULT ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );
    m_colorMap[ ColorTag::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ ColorTag::ACTIVE ] = osg::Vec4f( 0.7, 0.7, 0.7, 1.0 );
    m_colorMap[ ColorTag::OTHER ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );

    m_color = new osg::Uniform( "color", GetColor( ColorTag::DEFAULT ) );
    
    CreateDefaultShader();
}
////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger(
    const Dragger& dragger, const osg::CopyOp& copyop )
    :
    osg::MatrixTransform( dragger, copyop ),
    m_axesFlag( dragger.m_axesFlag ),
    m_transformationType( dragger.m_transformationType ),
    m_comboForm( dragger.m_comboForm ),
    m_colorMap( dragger.m_colorMap ),
    m_localToWorld( dragger.m_localToWorld ),
    m_worldToLocal( dragger.m_worldToLocal ),
    m_color( dragger.m_color ),
    m_parentManipulator( dragger.m_parentManipulator )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Dragger::~Dragger()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const Dragger* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::className() const
{
    return "Dragger";
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ComboForm()
{
    m_comboForm = true;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::DefaultForm()
{
    m_comboForm = false;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Drag( const osgUtil::LineSegmentIntersector& deviceInput )
{
    ManipFunction( deviceInput );

    return this;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Focus( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *(++npItr);
    if( this == node )
    {
        UseColor( ColorTag::FOCUS );

        --npItr;
        return this;
    }

    UseColor( ColorTag::DEFAULT );

    --npItr;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const AxesFlag::Enum Dragger::GetAxesFlag() const
{
    return m_axesFlag;
}
////////////////////////////////////////////////////////////////////////////////
const TransformationType::Enum Dragger::GetTransformationType() const
{
    return m_transformationType;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Push(
    const osgUtil::LineSegmentIntersector& deviceInput,
    const osg::NodePath& np,
    osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *(++npItr);
    if( this == node )
    {
        UseColor( ColorTag::ACTIVE );

        //Compute local to world and world to local matrices
        //For the dragger
        m_localToWorld = osg::computeLocalToWorld( np );
        m_worldToLocal = osg::Matrix::inverse( m_localToWorld );
        //For all associated node's transforms
        const std::vector< osg::Transform* >& associatedTransforms =
            m_parentManipulator->GetAssociatedTransforms();
        std::vector< osg::Transform* >::const_iterator itr =
            associatedTransforms.begin();
        for( itr; itr != associatedTransforms.end(); ++itr )
        {
            vxs::LocalToWorldNodePath nodePath(
                *itr, vxs::SceneManager::instance()->GetModelRoot() );
            vxs::LocalToWorldNodePath::NodeAndPathList npl =
                nodePath.GetLocalToWorldNodePath();
            vxs::LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );

            osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
            osg::Matrixd worldToLocal = osg::Matrix::inverse( localToWorld );
            m_associatedMatrices[ *itr ] =
                std::make_pair( localToWorld, worldToLocal );
        }

        //Turn off automatic scaling for the dragger
        osg::AutoTransform* autoTransform =
            static_cast< osg::AutoTransform* >(
                m_parentManipulator->getParent( 0 ) );
        autoTransform->setAutoScaleToScreen( false );

        m_startPosition = autoTransform->getPosition();

        //Get the start projected point
        ComputeProjectedPoint( deviceInput, m_startProjectedPoint );

        --npItr;
        return this;
    }

    TurnOff();

    --npItr;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *(++npItr);
    if( this == node )
    {
        UseColor( ColorTag::DEFAULT );

        //Clear the associated matrices
        m_associatedMatrices.clear();

        osg::AutoTransform* autoTransform =
            static_cast< osg::AutoTransform* >(
                m_parentManipulator->getParent( 0 ) );
        autoTransform->setAutoScaleToScreen( true );
        //Force update now on release event for this frame
        //This function call sets _firstTimeToInitEyePoint = true
        autoTransform->setAutoRotateMode( osg::AutoTransform::NO_ROTATION );

        --npItr;
        return this;
    }

    TurnOn();

    --npItr;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::CreateDefaultShader()
{
    //Create the shader used to render the dragger
    std::string fragmentSource =
    "uniform vec4 color; \n"

    "void main() \n"
    "{ \n"
        "gl_FragData[ 0 ] = color; \n"
        "gl_FragData[ 1 ] = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"
    "} \n";

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );

    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    stateSet->addUniform( m_color.get() );
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4& Dragger::GetColor( ColorTag::Enum colorTag )
{
    std::map< ColorTag::Enum, osg::Vec4 >::iterator itr =
        m_colorMap.find( colorTag );
    /*
    if( itr == m_colorMap.end() )
    {
        //error handling, but shouldn't need it since we know colors
    }
    */

    return itr->second;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Plane Dragger::GetPlane() const
{
    //The unit plane
    //| i   j   k  |
    //| Ax  Ay  Az |
    //| Bx  By  Bz |
    //A x B = ( AyBz - ByAz )i + ( BxAz - AxBz )j + ( AxBy - BxAy )k
    //P1 = ( 0, 0, 0 ) : P2 = ( 1, 0, 0 ) : P3 = ( 0, 0, 1 )
    //N = P1P2 x P1P3 = ( 1, 0, 0 ) x ( 0, 0, 1 ) = ( 0, -1, 0 )
    //-y + d = 0 : d = 0

    osg::Plane plane;
    switch( m_axesFlag )
    {
        case AxesFlag::X:
        case AxesFlag::YZ:
        {
            plane.set( osg::Vec3d( 1.0, 0.0, 0.0 ), 0.0 );

            break;
        }

        case AxesFlag::Y:
        case AxesFlag::XZ:
        {
            plane.set( osg::Vec3d( 0.0, 1.0, 0.0 ), 0.0 );

            break;
        }

        case AxesFlag::Z:
        case AxesFlag::XY:
        {
            plane.set( osg::Vec3d( 0.0, 0.0, 1.0 ), 0.0 );

            break;
        }
    }

    /*
    //plane = ( mVectorSpace == VectorSpace.Local )
    ? (Plane.Transform(p, Matrix.CreateFromQuaternion(mTransform.Rotation)
    * Matrix.CreateTranslation(mTransform.Translation)))
    : (Plane.Transform(p, Matrix.CreateTranslation(mTransform.Translation)));

    if( mVectorSpace == VectorSpace.Local )
    {

    }
    else
    {

    }
    */

    plane.transformProvidingInverse( m_worldToLocal );

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetUnitAxis( const bool& transformToWorld ) const
{
    osg::Vec3d zero( 0.0, 0.0, 0.0 );
    osg::Vec3d unitAxis = zero;
    if( ( m_axesFlag & AxesFlag::X ) == AxesFlag::X )
    {
        unitAxis.set( 1.0, 0.0, 0.0 );
    }
    if( ( m_axesFlag & AxesFlag::Y ) == AxesFlag::Y )
    {
        unitAxis.set( 0.0, 1.0, 0.0 );
    }
    if( ( m_axesFlag & AxesFlag::Z ) == AxesFlag::Z )
    {
        unitAxis.set( 0.0, 0.0, 1.0 );
    }

    if( unitAxis == zero )
    {
        //Error output
    }

    if( transformToWorld )
    {
        unitAxis = unitAxis * m_localToWorld;
    }

    return unitAxis;
}
////////////////////////////////////////////////////////////////////////////////
const bool Dragger::IntersectsPlane(
    const osg::Vec3d& startPosition,
    const osg::Vec3d& direction,
    double& intersectDistance )
{
    double error = -1E-05;

    osg::Plane plane = GetPlane();

    double num2 = plane.dotProductNormal( direction );
    if( fabs( num2 ) < error )
    {
        return false;
    }

    double num3 = plane.dotProductNormal( startPosition );
    intersectDistance = ( -plane[ 3 ] - num3 ) / num2;
    if( intersectDistance < 0.0 )
    {
        if( intersectDistance < error )
        {
            return false;
        }

        intersectDistance = 0.0;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetColor( ColorTag::Enum colorTag, osg::Vec4 newColor, bool use )
{
    osg::Vec4& color = GetColor( colorTag );
    if( color == newColor )
    {
        return;
    }

    color = newColor;

    if( use )
    {
        UseColor( colorTag );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::TurnOff()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::TurnOn()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UseColor( ColorTag::Enum colorTag )
{
    m_color->set( GetColor( colorTag ) );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetDrawableToAlwaysCull( osg::Drawable& drawable )
{
    osg::ref_ptr< ForceCullCallback > forceCullCallback =
        new ForceCullCallback();
    drawable.setCullCallback( forceCullCallback.get() );
}
////////////////////////////////////////////////////////////////////////////////
Dragger::ForceCullCallback::ForceCullCallback()
    :
    osg::Drawable::CullCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Dragger::ForceCullCallback::ForceCullCallback(
    const ForceCullCallback& forceCullCallback,
    const osg::CopyOp& copyop )
    :
    osg::Drawable::CullCallback( forceCullCallback, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::ForceCullCallback::cull(
    osg::NodeVisitor* nv,
    osg::Drawable* drawable,
    osg::RenderInfo* renderInfo ) const
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
