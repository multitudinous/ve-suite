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

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

// --- OSG Includes --- //
#include <osg/Drawable>
#include <osg/MatrixTransform>

#include <osgUtil/CullVisitor>
#include <osgUtil/LineSegmentIntersector>

// --- osgBullet Includes --- //
#include <osgBullet/AbsoluteModelTransform.h>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const TransformationType::Enum& transformationType )
    :
    osg::PositionAttitudeTransform(),
    m_transformationType( transformationType ),
    m_vectorSpace( VectorSpace::GLOBAL ),
    m_enabled( false ),
    m_comboForm( false ),
    m_startProjectedPoint( 0.0, 0.0, 0.0 ),
    m_endProjectedPoint( 0.0, 0.0, 0.0 ),
    m_rootDragger( NULL ),
    m_color( NULL )
{
    m_rootDragger = this;

    m_colorMap[ Color::DEFAULT ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );
    m_colorMap[ Color::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ Color::ACTIVE ] = osg::Vec4f( 0.7, 0.7, 0.7, 1.0 );
    m_colorMap[ Color::OTHER ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );

    m_color = new osg::Uniform( "color", GetColor( Color::DEFAULT ) );
    
    CreateDefaultShader();
}
////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const Dragger& dragger, const osg::CopyOp& copyop )
    :
    osg::PositionAttitudeTransform( dragger, copyop ),
    m_transformationType( dragger.m_transformationType ),
    m_vectorSpace( dragger.m_vectorSpace ),
    m_enabled( dragger.m_enabled ),
    m_comboForm( dragger.m_comboForm ),
    m_startProjectedPoint( dragger.m_startProjectedPoint ),
    m_endProjectedPoint( dragger.m_endProjectedPoint ),
    m_rootDragger( dragger.m_rootDragger ),
    m_colorMap( dragger.m_colorMap ),
    m_color( dragger.m_color )
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
    if( m_comboForm )
    {
        return;
    }

    m_comboForm = true;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::Connect( osg::Transform* activeAssociation )
{
    //Store the active association
    m_activeAssociation = activeAssociation;

    //Associate transform with this dragger
    m_associationSet.insert( activeAssociation );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    m_comboForm = false;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Disconnect()
{
    m_associationSet.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Enable( const bool& enable )
{
    m_enabled = enable;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Drag( const osgUtil::LineSegmentIntersector& deviceInput )
{
    //Get the end projected point
    if( !ComputeProjectedPoint( deviceInput, m_endProjectedPoint ) )
    {
        return NULL;
    }

    ComputeDeltaTransform();

    UpdateAssociations();

    //Reset
    m_startProjectedPoint = m_endProjectedPoint;

    return this;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Focus( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        UseColor( Color::FOCUS );

        return this;
    }

    UseColor( Color::DEFAULT );

    return NULL;
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
    osg::Node* node = *npItr;
    if( this == node )
    {
        UseColor( Color::ACTIVE );

        //Compute local to world and world to local matrices for dragger
        m_localToWorld = osg::computeLocalToWorld( np );
        m_worldToLocal = osg::Matrix::inverse( m_localToWorld );

        //Get the start projected point
        ComputeProjectedPoint( deviceInput, m_startProjectedPoint );

        //Compute the association matrices
        ComputeAssociationMatrices();

        return this;
    }

    Hide();

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        UseColor( Color::DEFAULT );

        //Clear the associated matrices
        m_associationMatricesMap.clear();

        return this;
    }

    Show();

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ComputeAssociationMatrices()
{
    //Compute local to world and world to local matrices for associated transforms
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( itr; itr != m_associationSet.end(); ++itr )
    {
        LocalToWorldNodePath nodePath(
            *itr, SceneManager::instance()->GetModelRoot() );
        LocalToWorldNodePath::NodeAndPathList npl =
            nodePath.GetLocalToWorldNodePath();
        LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );

        osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
        osg::Matrixd worldToLocal = osg::Matrixd::inverse( localToWorld );
        m_associationMatricesMap[ *itr ] =
            std::make_pair( localToWorld, worldToLocal );
    }
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
void Dragger::UpdateAssociations()
{
    //Set all associated transforms
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( itr; itr != m_associationSet.end(); ++itr )
    {
        osg::Transform* transform = *itr;
        AssociationMatricesMap::const_iterator ammItr =
            m_associationMatricesMap.find( transform );
        if( ammItr == m_associationMatricesMap.end() )
        {
            //Error output
            return;
        }

        const std::pair< osg::Matrixd, osg::Matrixd >& matrices = ammItr->second;

        osg::MatrixTransform* mt( NULL );
        osg::PositionAttitudeTransform* pat( NULL );
        osgBullet::AbsoluteModelTransform* amt( NULL );
        if( mt = transform->asMatrixTransform() )
        {
            const osg::Matrix& currentMatrix = mt->getMatrix();
            switch( m_transformationType )
            {
            /*
            mt->setMatrix(
                localToWorld *
                osg::Matrix::translate( deltaTranslation ) * currentMatrix *
                worldToLocal );
            */
            }
        }
        else if( pat = transform->asPositionAttitudeTransform() )
        {
            switch( m_transformationType )
            {
            case TransformationType::TRANSLATE_AXIS:
            case TransformationType::TRANSLATE_PAN:
            {
                osg::Vec3d position = pat->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                pat->setPosition( position );

                break;
            }
            case TransformationType::ROTATE_AXIS:
            case TransformationType::ROTATE_TWIST:
            {
                osg::Quat rotation = pat->getAttitude();
                rotation *= matrices.first.getRotate();
                rotation *= m_deltaRotation;
                rotation *= matrices.second.getRotate();
                pat->setAttitude( rotation );

                break;
            }
            default:
            {
                break;
            }
            } //end switch( m_transformationType )
        }
        else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform ) )
        {
            ;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4& Dragger::GetColor( Color::Enum colorTag )
{
    std::map< Color::Enum, osg::Vec4 >::iterator itr =
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
const osg::Plane Dragger::GetPlane( const bool& transform ) const
{
    //The unit plane
    //| i   j   k  |
    //| Ax  Ay  Az |
    //| Bx  By  Bz |
    //A x B = ( AyBz - ByAz )i + ( BxAz - AxBz )j + ( AxBy - BxAy )k
    //P1 = ( 0, 0, 0 ) : P2 = ( 1, 0, 0 ) : P3 = ( 0, 0, 1 )
    //N = P1P2 x P1P3 = ( 1, 0, 0 ) x ( 0, 0, 1 ) = ( 0, -1, 0 )
    //-y + d = 0 : d = 0

    osg::Plane plane( osg::Vec3d( 1.0, 0.0, 0.0 ), 0.0 );
    if( transform )
    {
        switch( m_vectorSpace )
        {
        case VectorSpace::GLOBAL:
        {
            osg::Vec4d vec = plane.asVec4();
            const osg::Vec3d trans = m_worldToLocal.getTrans();
            osg::Matrixd matrix;
            matrix.makeTranslate( trans );
            plane.transformProvidingInverse( matrix );

            plane.transformProvidingInverse( m_worldToLocal );
            
            break;
        }
        case VectorSpace::LOCAL:
        {
            plane.transformProvidingInverse( m_worldToLocal );

            break;
        }
        case VectorSpace::VIEW:
        {
            break;
        }
        } //end switch( m_vectorSpace )
    }

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetUnitAxis(
    const bool& zero, const bool& transform ) const
{
    osg::Vec3d unitAxis( 0.0, 0.0, 0.0 );
    if( !zero )
    {
        unitAxis.set( 1.0, 0.0, 0.0 );
    }

    if( transform )
    {
        switch( m_vectorSpace )
        {
        case VectorSpace::GLOBAL:
        {
            //unitAxis += m_localToWorld.getTrans();
            unitAxis = m_localToWorld * unitAxis;

            break;
        }
        case VectorSpace::LOCAL:
        {
            unitAxis = m_localToWorld * unitAxis;

            break;
        }
        case VectorSpace::VIEW:
        {
            break;
        }
        } //end switch( m_vectorSpace )
    }

    return unitAxis;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum& Dragger::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
const bool Dragger::IsCompound() const
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
const bool& Dragger::IsEnabled() const
{
    return m_enabled;
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
void Dragger::SetColor( Color::Enum colorTag, osg::Vec4 newColor, bool use )
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
void Dragger::SetRootDragger( Dragger* rootDragger )
{
    m_rootDragger = rootDragger;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetVectorSpace( const VectorSpace::Enum& vectorSpace )
{
    m_vectorSpace = vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Hide()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Show()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UseColor( Color::Enum colorTag )
{
    m_color->set( GetColor( colorTag ) );
}
////////////////////////////////////////////////////////////////////////////////
class ForceCullCallback : public osg::Drawable::CullCallback
{
public:
    ///
    ForceCullCallback()
        :
        osg::Drawable::CullCallback()
    {
        ;
    }

    ///
    ForceCullCallback(
        const ForceCullCallback& forceCullCallback,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY )
        :
        osg::Drawable::CullCallback( forceCullCallback, copyop )
    {
        ;
    }

    ///
    META_Object(
        ves::xplorer::scenegraph::manipulator::Dragger, ForceCullCallback );

    ///
    virtual bool cull(
        osg::NodeVisitor* nv,
        osg::Drawable* drawable,
        osg::RenderInfo* renderInfo ) const
    {
        return true;
    }

protected:

private:

};
////////////////////////////////////////////////////////////////////////////////
void ves::xplorer::scenegraph::manipulator::SetDrawableToAlwaysCull(
    osg::Drawable& drawable )
{
    osg::ref_ptr< ForceCullCallback > fcc = new ForceCullCallback();
    drawable.setCullCallback( fcc.get() );
}
////////////////////////////////////////////////////////////////////////////////
