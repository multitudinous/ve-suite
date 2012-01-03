/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/communication/CommunicationHandler.h>
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/GhostController.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/Transform.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/AutoTransform>
#include <osgUtil/CullVisitor>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- osgBullet Includes --- //
#include <osgwTools/AbsoluteModelTransform.h>

#include <osgbCollision/RefBulletObject.h>
#include <osgbCollision/Utils.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const TransformationType::Enum& transformationType )
    :
    AutoTransform(),
    m_transformationType( transformationType ),
    m_vectorSpace( VectorSpace::GLOBAL ),
    m_axisDirection( AxisDirection::POSITIVE ),
    m_enabled( false ),
    m_comboForm( false ),
    m_startProjectedPoint( 0.0, 0.0, 0.0 ),
    m_endProjectedPoint( 0.0, 0.0, 0.0 ),
    m_rootDragger( NULL ),
    m_deltaRotation( 0.0, 0.0, 0.0, 0.0 ),
    m_deltaTranslation( 0.0, 0.0, 0.0 ),
    m_deltaScale( 0.0, 0.0, 0.0 ),
    m_physicsSimulator( *scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *scenegraph::SceneManager::instance() ),
    m_isRootDragger( true ),
    m_activeAssociation( 0 )
{
    m_rootDragger = this;

    m_colorMap[ Color::DEFAULT ] = osg::Vec4f( 1.0, 0.0, 0.0, 1.0 );
    m_colorMap[ Color::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ Color::ACTIVE ] = osg::Vec4f( 1.0, 0.0, 1.0, 1.0 );
    m_colorMap[ Color::DISABLED ] = osg::Vec4f( 0.6, 0.6, 0.6, 1.0 );
    m_colorMap[ Color::OTHER ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );

    m_color = new osg::Uniform( "color", GetColor( Color::DEFAULT ) );

    CreateDefaultShader();
}
////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const Dragger& dragger, const osg::CopyOp& copyop )
    :
    AutoTransform( dragger, copyop ),
    m_transformationType( dragger.m_transformationType ),
    m_vectorSpace( dragger.m_vectorSpace ),
    m_axisDirection( dragger.m_axisDirection ),
    m_enabled( dragger.m_enabled ),
    m_comboForm( dragger.m_comboForm ),
    m_startProjectedPoint( dragger.m_startProjectedPoint ),
    m_endProjectedPoint( dragger.m_endProjectedPoint ),
    m_rootDragger( dragger.m_rootDragger ),
    m_deltaRotation( dragger.m_deltaRotation ),
    m_deltaTranslation( dragger.m_deltaTranslation ),
    m_deltaScale( dragger.m_deltaScale ),
    m_physicsSimulator( *scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *scenegraph::SceneManager::instance() ),
    m_isRootDragger( dragger.m_isRootDragger ),
    m_activeAssociation( dragger.m_activeAssociation ),
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
void Dragger::accept( osg::NodeVisitor& nv )
{
    //Now do the proper accept
    AutoTransform::accept( nv );
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis* Dragger::AsTranslateAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePlane* Dragger::AsTranslatePlane()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePan* Dragger::AsTranslatePan()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Rotate* Dragger::AsRotate()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis* Dragger::AsRotateAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist* Dragger::AsRotateTwist()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis* Dragger::AsScaleAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
ScaleUniform* Dragger::AsScaleUniform()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
CompoundDragger* Dragger::AsCompoundDragger()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const Dragger* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::className() const
{
    return "Transform";
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::libraryName() const
{
    return "osg";
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
    //Check and see if the selected transform has an attached physics mesh
    osgwTools::AbsoluteModelTransform* amt =
        dynamic_cast< osgwTools::AbsoluteModelTransform* >(
            activeAssociation->getParent( 0 ) );
    if( amt )
    {
        activeAssociation = amt;
        //This is hacky
        if( m_isRootDragger )
        {
            osgbCollision::RefRigidBody* refRB =
                static_cast< osgbCollision::RefRigidBody* >( amt->getUserData() );
            if( !refRB )
            {
                return false;
            }

            btRigidBody* btRB = refRB->get();
            if( !btRB )
            {
                return false;
            }

            PhysicsRigidBody* phyRB =
                static_cast< PhysicsRigidBody* >( btRB->getUserPointer() );
            if( !phyRB )
            {
                return false;
            }
            phyRB->EnableGhostControl();
        }
    }

    //Store the active association
    m_activeAssociation = activeAssociation;
    m_associationSet.insert( m_activeAssociation );

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
    m_activeAssociation = NULL;

    //This is hacky
    if( m_isRootDragger )
    {
        AssociationSet::const_iterator itr = m_associationSet.begin();
        for( ; itr != m_associationSet.end(); ++itr )
        {
            osgwTools::AbsoluteModelTransform* amt =
                dynamic_cast< osgwTools::AbsoluteModelTransform* >( *itr );
            if( !amt )
            {
                continue;
            }

            osgbCollision::RefRigidBody* refRB =
                static_cast< osgbCollision::RefRigidBody* >( amt->getUserData() );
            if( !refRB )
            {
                continue;
            }

            btRigidBody* btRB = refRB->get();
            if( !btRB )
            {
                continue;
            }

            PhysicsRigidBody* phyRB =
                static_cast< PhysicsRigidBody* >( btRB->getUserPointer() );
            if( !phyRB )
            {
                continue;
            }
            phyRB->EnableGhostControl( false );
        }
    }

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
    ComputeProjectedPoint( deviceInput, m_endProjectedPoint );

    //Compute the delta transform for the drag
    ComputeDeltaTransform();

    //Update all associated matrices with the delta transform
    UpdateAssociations();

    //Custom Event::Drag functionality
    CustomDragAction();

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
        //Use focused color if this is focused
        UseColor( Color::FOCUS );

        //Custom Event::Focus functionality
        CustomFocusAction();

        //This is in the node path
        return this;
    }

    //Use default color if this is not focused
    UseColor( Color::DEFAULT );

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
TransformationType::Enum Dragger::GetTransformationType() const
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
        //Tell root dragger to stop auto scaling
        //m_rootDragger->SetAutoScaleToScreen( false );

        //Use active color if this is active
        UseColor( Color::ACTIVE );

        //Compute local to world and world to local matrices for this
        m_localToWorld = osg::computeLocalToWorld( np );
        m_worldToLocal = osg::Matrix::inverse( m_localToWorld );

        //Compute the associated ltw and wtl matrices
        ComputeAssociationMatrices();

        //Get the start projected point
        ComputeProjectedPoint( deviceInput, m_startProjectedPoint );

        //Custom Event::Push functionality
        CustomPushAction();

        //This is in the node path
        return this;
    }

    //Hide if this is not active
    Hide();

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        //Tell root dragger to auto scale
        //m_rootDragger->SetAutoScaleToScreen( true );
        //Force update now on release event for this frame
        //m_rootDragger->SetAutoRotateMode( m_rootDragger->GetAutoRotateMode() );

        //Use default color if this is active
        UseColor( Color::DEFAULT );

        //Custom Event::Release functionality
        CustomReleaseAction();

        //Clear the associated matrices
        m_associationMatricesMap.clear();

        //Reset the start and end points
        m_startProjectedPoint.set( 0.0, 0.0, 0.0 );
        m_endProjectedPoint.set( 0.0, 0.0, 0.0 );

        //This is in the node path
        return this;
    }

    //Show if this is not active
    Show();

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ComputeAssociationMatrices()
{
    //Compute local to world and world to local matrices for associated transforms
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( ; itr != m_associationSet.end(); ++itr )
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
    if( m_associationMatricesMap.size() == 0 )
    {
        std::cout << "Dragger::UpdateAssociations There is no data for " 
            << "this dragger to map to." << std::endl;
        return;
    }

    //Set all associated transforms
    AssociationSet::const_iterator asItr = m_associationSet.begin();
    for( ; asItr != m_associationSet.end(); ++asItr )
    {
        osg::Transform* transform = *asItr;
        AssociationMatricesMap::const_iterator ammItr =
            m_associationMatricesMap.find( transform );
        if( ammItr == m_associationMatricesMap.end() )
        {
            //Error output
            continue;
        }
        std::pair< osg::Matrixd, osg::Matrixd > const& matrices = ammItr->second;

        //Test for physics transforms 1st
        osgwTools::AbsoluteModelTransform* amt =
            dynamic_cast< osgwTools::AbsoluteModelTransform* >( transform );
        if( amt )
        {
            osgbCollision::RefRigidBody* refRB =
                static_cast< osgbCollision::RefRigidBody* >( amt->getUserData() );
            if( !refRB )
            {
                continue;
            }

            btRigidBody* btRB = refRB->get();
            if( !btRB )
            {
                continue;
            }

            PhysicsRigidBody* phyRB =
                static_cast< PhysicsRigidBody* >( btRB->getUserPointer() );
            if( !phyRB )
            {
                continue;
            }

            GhostController& ghostController = phyRB->GetGhostController();

            bool currentIdle = m_physicsSimulator.GetIdle();
            m_physicsSimulator.SetIdle( true );

            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                btVector3 deltaTranslation =
                    osgbCollision::asBtVector3( m_deltaTranslation );
                ghostController.Translate( deltaTranslation, currentIdle );
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                btQuaternion deltaRotation(
                    m_deltaRotation.x(), m_deltaRotation.y(),
                    m_deltaRotation.z(), m_deltaRotation.w() );
                ghostController.Rotate( deltaRotation, currentIdle );
            }

            m_physicsSimulator.SetIdle( currentIdle );

            continue;
        }

        //Test for PATs 2nd
        osg::PositionAttitudeTransform* pat =
            transform->asPositionAttitudeTransform();
        if( pat )
        {
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                /*
                osg::Vec3d translation =
                    pat->getPosition() +
                    matrices.first.getTrans() +
                    m_deltaTranslation +
                    matrices.second.getTrans();
                pat->setPosition( translation );
                */
                osg::Vec3d position = pat->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                pat->setPosition( position );
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                osg::Quat rotation =
                    pat->getAttitude() *
                    matrices.first.getRotate() *
                    m_deltaRotation *
                    matrices.second.getRotate();
                pat->setAttitude( rotation );
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                osg::Vec3d scale = pat->getScale();
                scale = scale * matrices.first;
                scale += m_deltaScale;
                scale = scale * matrices.second;
                pat->setScale( scale );
            }

            scenegraph::DCS* tempDCS = static_cast< scenegraph::DCS* >( pat );
            if( tempDCS )
            {
                //UpdateConductorData( tempDCS );
            }

            continue;
        }

        //Test for MTs 3rd
        osg::MatrixTransform* mt = transform->asMatrixTransform();
        if( mt )
        {
            //const osg::Matrix& currentMatrix = mt->getMatrix();
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                ;
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                ;
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                ;
            }

            continue;
        }

        //Test for ATs 4th
        osg::AutoTransform* at =
            dynamic_cast< osg::AutoTransform* >( transform );
        if( at )
        {
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                osg::Vec3d position = at->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                at->setPosition( position );
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                osg::Quat rotation = at->getRotation();
                rotation *= matrices.first.getRotate();
                rotation *= m_deltaRotation;
                rotation *= matrices.second.getRotate();
                at->setRotation( rotation );
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                ;
            }

            continue;
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
const osg::Plane Dragger::GetPlane( const bool& parallel ) const
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
    if( parallel )
    {
        //If rotate to screen
        osg::Vec3d t, s;
        osg::Quat r, so;
        m_currentGLTransformInfo->GetViewMatrixOSG().decompose(
            t, r, s, so );
        osg::Vec3d normal = GetUnitAxis() * osg::Matrix::rotate( r.inverse() );
        normal.normalize();
        plane.set( normal, m_rootDragger->GetPosition() );

        //If rotate to eye
        //plane.set(
            //m_rootDragger->GetPreviousEyePoint() - m_rootDragger->GetPosition(),
            //m_rootDragger->GetPosition() );
    }
    else
    {
        plane.set( GetUnitAxis(), 0.0 );
        plane.transformProvidingInverse( m_worldToLocal );
    }

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetAxis( const bool& premultiply ) const
{
    osg::Vec3d axis = GetUnitAxis();
    if( premultiply )
    {
        return axis * m_localToWorld;
    }

    return m_localToWorld * axis;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetUnitAxis() const
{
    osg::Vec3d unitAxis( 0.0, 0.0, 1.0 );

    return unitAxis;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Plane Dragger::GetUnitPlane() const
{
    osg::Plane plane( GetUnitAxis(), 0.0 );

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum& Dragger::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
const bool& Dragger::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetAxisDirection( const AxisDirection::Enum& axisDirection )
{
    m_axisDirection = axisDirection;
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
    if( this != rootDragger )
    {
        m_rootDragger = rootDragger;
        m_isRootDragger = false;
    }
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
void Dragger::UpdateConductorData( ves::xplorer::scenegraph::DCS* dcs )
{
    ves::open::xml::model::ModelPtr model = dcs->GetModelData();
    if( !model )
    {
        //std::cout << "Null Model " << std::endl;
        return;
    }
    ves::open::xml::cad::CADNodePtr cadNode = dcs->GetCADPart();
    if( !cadNode )
    {
        //std::cout << "Null CAD Node " << std::endl;
        return;
    }
    ves::open::xml::TransformPtr tempTransform = cadNode->GetTransform();

    if( !model->GetParentSystem() )
    {
        //std::cout << "Null System " << std::endl;
        return;
    }
    
    ///Get command
    ves::open::xml::CommandPtr modelUpdateData( new ves::open::xml::Command() );
    modelUpdateData->SetCommandName( "MODEL_DATA_UPDATE" );

    //Create the uuid for the model of the transform
    ves::open::xml::DataValuePairPtr parentSystemId( new ves::open::xml::DataValuePair() );
    parentSystemId->SetData( "PARENT_SYSTEM_ID", model->GetParentSystem()->GetID() );
    modelUpdateData->AddDataValuePair( parentSystemId );
    
    //Create the uuid for the plugin of the transform
    ves::open::xml::DataValuePairPtr pluginId( new ves::open::xml::DataValuePair() );
    pluginId->SetData( "PLUGIN_ID", model->GetID() );
    modelUpdateData->AddDataValuePair( pluginId );

    ves::open::xml::CommandPtr transformData( new ves::open::xml::Command() );
    transformData->SetCommandName( "MODEL_DATA" );

    ves::open::xml::DataValuePairPtr cadTransformDVP( new ves::open::xml::DataValuePair() );
    cadTransformDVP->SetData( "CAD_TRANSFORM", tempTransform );
    transformData->AddDataValuePair( cadTransformDVP );

    ves::open::xml::DataValuePairPtr cadIdDVP( new ves::open::xml::DataValuePair() );
    cadIdDVP->SetData( "CAD_ID", cadNode->GetID() );
    transformData->AddDataValuePair( cadIdDVP );

    ves::open::xml::DataValuePairPtr pluginDataDVP( new ves::open::xml::DataValuePair() );
    pluginDataDVP->SetData( "PLUGIN_DATA", transformData );

    modelUpdateData->AddDataValuePair( pluginDataDVP );

    ves::xplorer::communication::CommunicationHandler::instance()->
        SetXMLCommand( modelUpdateData );
}
////////////////////////////////////////////////////////////////////////////////

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves
