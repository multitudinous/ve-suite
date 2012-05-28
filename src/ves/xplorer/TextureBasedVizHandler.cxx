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
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DataSet.h>

#include <ves/xplorer/event/volume/TBTransientDurationUpdateEH.h>
#include <ves/xplorer/event/volume/TBTransientModeUpdateEH.h>
#include <ves/xplorer/event/volume/TBIsosurfaceUpdateEH.h>
#include <ves/xplorer/event/volume/TBIsosurfaceEnableEH.h>
#include <ves/xplorer/event/volume/TBClipPlaneEH.h>
#include <ves/xplorer/event/volume/TBBBoxEH.h>
#include <ves/xplorer/event/volume/TBUpdateScalarRangeEH.h>
#include <ves/xplorer/event/volume/TBUpdateSolutionEH.h>
#include <ves/xplorer/event/volume/TBActivateEH.h>
#include <ves/xplorer/event/volume/TBSetActiveShaderManagerEH.h>
#include <ves/xplorer/event/volume/TBSliceNumberUpdateEH.h>
#include <ves/xplorer/event/volume/TBPhongShadingEnableEH.h>
#include <ves/xplorer/event/volume/TBPreIntegrateEH.h>

#include <ves/xplorer/event/volume/VolumeVisSlotInitializer.h>

#include <ves/xplorer/event/viz/cfdGraphicsObject.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>

#include <ves/xplorer/volume/cfdVolumeVisNodeHandler.h>
#include <ves/xplorer/volume/cfdTextureDataSet.h>
#include <ves/xplorer/volume/cfdTextureManager.h>
#include <ves/xplorer/volume/cfdVolumeVisualization.h>

#include <osg/State>
//#include <osgUtil/SceneView>
#include <osgDB/WriteFile>

#include <fstream>

#include <ves/xplorer/volume/cfdScalarVolumeVisHandler.h>
#include <ves/xplorer/volume/cfdScalarShaderManager.h>
#include <ves/xplorer/volume/cfdOSGTransferShaderManager.h>

#include <ves/xplorer/volume/cfdVectorVolumeVisHandler.h>
#include <ves/xplorer/volume/cfdOSGAdvectionShaderManager.h>

vprSingletonImpLifetime( ves::xplorer::TextureBasedVizHandler, 1 );
using namespace ves::xplorer::volume;
using namespace ves::xplorer;
using namespace ves::xplorer::command;

//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
TextureBasedVizHandler::TextureBasedVizHandler()
    :
    m_logger( Poco::Logger::get( "xplorer.TextureBasedVizHandler" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    _animationDelay = 0.0001f;
    _appTime = 0.0;
    _activeVolumeVizNode = 0;
    _activeTM = 0;
    _parent = 0;
    _currentBBox = 0;
    _cleared = true;
    _pbm = 0;

    _activeVisNodeHdlr = 0;
    _textureBaseSelected = false;
    _activeTDSet = 0;
    _svvh = 0;
    _vvvh = 0;
    m_isMaster = false;

    m_slotInitializer =
        ves::xplorer::event::volume::VolumeVisSlotInitializerPtr(
            new ves::xplorer::event::volume::VolumeVisSlotInitializer() );

    _eventHandlers[std::string( "TB_SET_ACTIVE_SHADER_MANAGER" )] = new ves::xplorer::event::TextureBasedSetActiveShaderManagerEventHandler();
    _eventHandlers[std::string( "TB_ACTIVATE" )] = new ves::xplorer::event::TextureBasedActivateEventHandler();
    _eventHandlers[std::string( "TB_ACTIVE_SOLUTION" )] = new ves::xplorer::event::TextureBasedUpdateSolutionEventHandler();
    _eventHandlers[std::string( "TB_SCALAR_RANGE" )] = new ves::xplorer::event::TextureBasedUpdateScalarRangeEventHandler();
    _eventHandlers[std::string( "TB_BBOX_DISPLAY" )] = new ves::xplorer::event::TextureBasedBoundingBoxEventHandler();
    _eventHandlers[std::string( "TB_ROI_UPDATE" )] = new ves::xplorer::event::TextureBasedClipPlaneEventHandler();
    _eventHandlers[std::string( "TB_ISOSURFACE_ENABLE" )] = new ves::xplorer::event::TextureBasedIsosurfaceEnableEventHandler();
    _eventHandlers[std::string( "TB_UPDATE_ISOSURFACE" )] = new ves::xplorer::event::TextureBasedIsosurfaceUpdateEventHandler();
    _eventHandlers[std::string( "TB_TRANSIENT_MODE_UPDATE" )] = new ves::xplorer::event::TextureBasedTransientModeUpdateEventHandler();
    _eventHandlers[std::string( "TB_TRANSIENT_DURATION_UPDATE" )] = new ves::xplorer::event::TextureBasedTransientDurationUpdateEventHandler();
    _eventHandlers[std::string( "TB_UPDATE_NUMBER_SLICE_PLANES" )] = new ves::xplorer::event::TextureBasedSliceNumberUpdateEventHandler();
    _eventHandlers[std::string( "TB_PHONG_SHADING_ENABLE" )] = new ves::xplorer::event::TextureBasedPhongShadingEnableEventHandler();
    _eventHandlers[std::string( "TB_FULL_PREINTEGRATE_UPDATE" )] = new ves::xplorer::event::TextureBasedPreIntegrateEnableEventHandler();
}
///////////////////////////////////////////////
TextureBasedVizHandler::~TextureBasedVizHandler( void )
{
    if( _currentBBox )
    {
        delete [] _currentBBox;
        _currentBBox = 0;
    }

    if( _svvh )
    {
        delete _svvh;
        _svvh = 0;
    }

    if( _vvvh )
    {
        delete _svvh;
        _vvvh = 0;
    }

    std::map< std::string, ves::xplorer::event::TextureBasedEventHandler*>::iterator pos;
    for( pos = _eventHandlers.begin(); pos != _eventHandlers.end(); )
    {
        delete pos->second;
        _eventHandlers.erase( pos++ );
    }
}
//////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetMasterNode( bool isMaster )
{
    m_isMaster = isMaster;
}
/////////////////////////////////////////////////
void TextureBasedVizHandler::_updateShaders()
{
    if( !_activeTM )
    {
        return;
    }

    if( _activeTM->GetDataType( 0 ) == cfdTextureManager::SCALAR )
    {
        _updateScalarVisHandler();
    }
    else if( _activeTM->GetDataType( 0 ) == cfdTextureManager::VECTOR )
    {
        _updateVectorVisHandler();
    }
}
//////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateTransientFrame()
{
    if( _activeTM )
    {
        _activeTM->CalculateUpdateTime( _appTime, _animationDelay );
        if( _activeTM->TimeToUpdate() )
        {
            _activeTM->getNextFrame();
        }
    }
}
////////////////////////////////////////////////////////////////////////
cfdTextureManager* TextureBasedVizHandler::GetActiveTextureManager()
{
    return _activeTM;
}
//////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateIsosurface( double value )
{
    if( _svvh )
    {
        cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
        if( sShader )
        {
            //sShader->FastTransferFunctionUpdate();
            sShader->FullTransferFunctionUpdate();

            sShader->ActivateIsoSurface();
            sShader->SetIsoSurfaceValue( value );
        }
    }
}
////////////////////////////////////////////////////////////
void TextureBasedVizHandler::EnsureIsosurface( bool onOff )
{
    if( _svvh )
    {
        cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
        if( sShader )
        {
            if( onOff )
            {
                sShader->ActivateIsoSurface();
            }
            else
            {
                sShader->DeactivateIsoSurface();
            }
            sShader->FullTransferFunctionUpdate();
            sShader->EnsureScalarRange();
        }
    }
}
//////////////////////////////////////////////////////////////
void TextureBasedVizHandler::EnsurePhongShading( bool onOff )
{
    if( _svvh )
    {
        cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
        if( sShader )
        {
            //this is hard coded and will need to change--biv
            if( onOff )
            {
                sShader->SetActiveShaderProgram( "Phong Lit Volume Render" );
            }
            else
            {
                sShader->SetActiveShaderProgram( "Basic Volume Render" );
            }
            sShader->FullTransferFunctionUpdate();
            sShader->EnsureScalarRange();

        }
    }
}
////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateClipPlane( std::string planeCoordinate,
        std::string planeDirection,
        double alpha )
{
    double plane[4] = {0, 0, 0, 0};
    if( planeCoordinate == "X" )
    {
        plane[0] = 1.0;
        plane[3] = _currentBBox[0] + alpha * ( _currentBBox[1] - _currentBBox[0] );
        //get the xplane positions
        if( planeDirection == "Positive" )
        {
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::XPLANE_MIN, plane );
        }
        else if( planeDirection == "Negative" )
        {
            plane[0] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::XPLANE_MAX, plane );
        }
    }
    else if( planeCoordinate == "Y" )
    {
        plane[1] = 1.0;
        plane[3] = _currentBBox[2] + alpha * ( _currentBBox[3] - _currentBBox[2] );
        //get the yplane positions
        if( planeDirection == "Positive" )
        {
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::YPLANE_MIN, plane );
        }
        else if( planeDirection == "Negative" )
        {
            plane[1] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::YPLANE_MAX, plane );
        }
    }
    else if( planeCoordinate == "Z" )
    {
        //create an z plane
        plane[2] = 1.0;
        plane[3] = _currentBBox[4] + alpha * ( _currentBBox[5] - _currentBBox[4] );
        //get the zplane positions
        if( planeDirection == "Positive" )
        {
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::ZPLANE_MIN, plane );
        }
        else if( planeDirection == "Negative" )
        {
            plane[2] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::ZPLANE_MAX, plane );
        }
    }
    else
    {
        std::cout << "Invalid Clipping plane coordinate specified!!" << std::endl;
        std::cout << "TextureBasedVizHandler::UpdateClipPlane" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateClipPlane( std::vector< double > const& roi )
{
    double plane[4] = {0, 0, 0, 0};
    //if( planeCoordinate == "X" )
    {
        plane[0] = 0.0;
        plane[1] = 0.0;
        plane[2] = 0.0;
        plane[3] = 0.0;
        plane[0] = 1.0;
        //get the xplane positions
        //if( planeDirection == "Positive" )
        {
            plane[3] = _currentBBox[0] + roi.at( 0 ) * ( _currentBBox[1] - _currentBBox[0] );
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::XPLANE_MIN, plane );
        }
        //else if( planeDirection == "Negative" )
        {
            plane[3] = _currentBBox[0] + roi.at( 1 ) * ( _currentBBox[1] - _currentBBox[0] );
            plane[0] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::XPLANE_MAX, plane );
        }
    }
    //else if( planeCoordinate == "Y" )
    {
        plane[0] = 0.0;
        plane[1] = 0.0;
        plane[2] = 0.0;
        plane[3] = 0.0;
        plane[1] = 1.0;
        //get the yplane positions
        //if( planeDirection == "Positive" )
        {
            plane[3] = _currentBBox[2] + roi.at( 2 ) * ( _currentBBox[3] - _currentBBox[2] );
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::YPLANE_MIN, plane );
        }
        //else if( planeDirection == "Negative" )
        {
            plane[3] = _currentBBox[2] + roi.at( 3 ) * ( _currentBBox[3] - _currentBBox[2] );
            plane[1] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::YPLANE_MAX, plane );
        }
    }
    //else if( planeCoordinate == "Z" )
    {
        //create an z plane
        plane[0] = 0.0;
        plane[1] = 0.0;
        plane[2] = 0.0;
        plane[3] = 0.0;
        plane[2] = 1.0;
        //get the zplane positions
        //if( planeDirection == "Positive" )
        {
            plane[3] = _currentBBox[4] + roi.at( 4 ) * ( _currentBBox[5] - _currentBBox[4] );
            plane[3] *= -1.0;
            plane[3] += .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::ZPLANE_MIN, plane );
        }
        //else if( planeDirection == "Negative" )
        {
            plane[3] = _currentBBox[4] + roi.at( 5 ) * ( _currentBBox[5] - _currentBBox[4] );
            plane[2] *= -1.0;
            plane[3] -= .001;
            _activeVolumeVizNode->UpdateClipPlanePosition( cfdVolumeVisualization::ZPLANE_MAX, plane );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateNumberOfSlicePlanes( unsigned int nSlices )
{
    if( _activeVolumeVizNode )
    {
        _activeVolumeVizNode->SetNumberOfSlices( nSlices );
    }
}
////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetTransientDirection( std::string direction )
{
    if( _activeTM )
    {
        _activeTM->setDirection( ( direction == "Forward" ) ? 1 : -1 );
    }
}

////////////////////////////////////////////////////////////
void TextureBasedVizHandler::StopTransientVisualization()
{
    if( _activeTM )
    {
        _activeTM->SetPlayMode( "Stop" );
    }

}
////////////////////////////////////////////////////////////
void TextureBasedVizHandler::PlayTransientVisualization()
{
    if( _activeTM )
    {
        _activeTM->SetPlayMode( "Play" );
        SetTransientDirection( "Forward" );
    }
}
////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateTransientDuration( double duration )
{
    if( !_activeTM )
    {
        return;
    }

    if( !_svvh )
    {
        return;
    }

    ///duration calculation
    unsigned int nTimesteps = _activeTM->numberOfFields();
    _animationDelay = duration / ( ( double )nTimesteps );
    cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
    if( sShader )
    {
        sShader->SetDelayTime( _animationDelay );
    }
}
/////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::StepTransientVisualization( std::string direction )
{
    if( !_activeTM )
    {
        return;
    }

    try
    {
        StopTransientVisualization();
        SetTransientDirection( direction );

        int curFrame = _activeTM->getNextFrame();
        if( _svvh )
        {
            cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
            if( sShader )
            {
                sShader->SetCurrentTransientTexture( curFrame, true );
            }
        }
    }
    catch( ... )
    {
        std::cout << "Texture Manager not set!!" << std::endl;
        std::cout << " TextureBasedVizHandler::StepTransientVisualization" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetCurrentFrame( unsigned int frame )
{
    try
    {
        StopTransientVisualization();
        _activeTM->setPlayMode( cfdTextureManager::STOP );
        _activeTM->SetCurrentFrame( frame );
    }
    catch( ... )
    {
        std::cout << "Texture Manager not set!!" << std::endl;
        std::cout << " TextureBasedVizHandler::SetCurrentFrame" << std::endl;
    }
}
//////////////////////////////////////////////////////
void TextureBasedVizHandler::_updateVisualization()
{

    /*if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ARBITRARY){
    if(_activeVolumeVizNode&&_currentBBox){
    //create an arbitrary plane
    double arbPlane[4] = {0,0,0,0};
    //not sure how this is going to work w/ the gui!!!!
    arbPlane
    _activeVolumeVizNode->AddClipPlane(cfdVolumeVisualization::ARBITRARY,arbPlane);
    }
    }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_STOP){
    if(_activeTM){
    _activeTM->setPlayMode(cfdTextureManager::STOP);
    }
    _cleared = false;
    }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_PLAY){
    if(_activeTM){
    _activeTM->setDirection(1);
    _activeTM->setPlayMode(cfdTextureManager::PLAY);
    }
    _cleared = false;
    }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_DURATION){
    if(_svvh){
    double duration = (double) _cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE);
    unsigned int nTimesteps = _activeTM->numberOfFields();
    _animationDelay = duration/((double)nTimesteps);
    cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
    if(sShader)
    {
    sShader->SetDelayTime(_animationDelay);
    }
    }
    }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_SET_FRAME){
    if(_activeTM){
    _activeTM->setPlayMode(cfdTextureManager::STOP);
    _activeTM->SetCurrentFrame((unsigned int)_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE));
    }
    _cleared = false;
    }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_FORWARD){
    if(_activeTM){
    _activeTM->setDirection(1);
    _activeTM->setPlayMode(cfdTextureManager::STOP);
    int curFrame = _activeTM->getNextFrame();
    if(_svvh){
    cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
    if(sShader)
    {
    sShader->SetCurrentTransientTexture(curFrame,false);
    }
    }

    }
    _cleared = false;
    }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_BACKWARD){
    if(_activeVolumeVizNode&&_activeTM){
    _activeTM->setDirection(-1);
    _activeTM->setPlayMode(cfdTextureManager::STOP);
    int curFrame = _activeTM->getNextFrame();
    if(_svvh){
    cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
    if(sShader)
    {
    sShader->SetCurrentTransientTexture(curFrame,false);
    }
    }
    if(_vvvh){
    _vvvh->SetCurrentTransientTexture(curFrame,false);
    }
    }
    _cleared = false;
    }*/
}
///////////////////////////////////////////
void TextureBasedVizHandler::UpdateGraph()
{
    if( !ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() )
    {
        return;
    }

    SetParentNode( static_cast< ves::xplorer::scenegraph::Group* >(
                       ModelHandler::instance()->GetActiveModel()->
                       GetActiveDataSet()->GetSwitchNode()->GetChild( 1 ) ) );
    SetActiveTextureDataSet( ModelHandler::instance()->GetActiveTextureDataSet() );

    //place vv node on the graph
    if( !_activeVolumeVizNode )
    {
        return;
    }

    LOG_INFO( "Adding TBET node to the graph." );

    osg::ref_ptr<osg::Group> tParent = _parent.get();
    osg::ref_ptr<osg::Switch> tVV = _activeVolumeVizNode->GetVolumeVisNode();
    if( !tParent->containsNode( tVV.get() ) )
    {
        tParent->addChild( tVV.get() );
    }
}
/////////////////////////////////////////
void TextureBasedVizHandler::ClearAll()
{
    if( !_parent.valid() )
    {
        return;
    }

    //need to remove the clip planes
    if( _activeVolumeVizNode )
    {
        _activeVolumeVizNode->ResetClipPlanes();
        _parent->removeChild( _activeVolumeVizNode->GetVolumeVisNode().get() );
        _activeVolumeVizNode = 0;
    }
    _activeTM = 0;
    _activeVolumeVizNode = 0;
    _parent = 0;
    _currentBBox = 0;
    _cleared = true;
    _pbm = 0;

    _activeVisNodeHdlr = 0;
    _textureBaseSelected = false;
    _activeTDSet = 0;
    _svvh = 0;
    _vvvh = 0;
}
///////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetCurrentTime( double time )
{
    _appTime = time;
}
////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateBoundingBox( bool value )
{
    if( _activeVolumeVizNode && _activeVisNodeHdlr )
    {
        if( value )
        {
            _activeVisNodeHdlr->TurnOnBBox();
        }
        else
        {
            _activeVisNodeHdlr->TurnOffBBox();
        }
    }
}
////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateActiveTextureManager()
{
    if( _activeTDSet )
    {
        _activeVolumeVizNode =  _activeTDSet->GetVolumeVisNode();
        _activeTM = _activeTDSet->GetActiveTextureManager();

        if( !_currentBBox )
        {
            _currentBBox = new float[6];
        }
        _currentBBox[0] = _activeTM->getBoundingBox()[0];
        _currentBBox[1] = _activeTM->getBoundingBox()[1];
        _currentBBox[2] = _activeTM->getBoundingBox()[2];
        _currentBBox[3] = _activeTM->getBoundingBox()[3];
        _currentBBox[4] = _activeTM->getBoundingBox()[4];
        _currentBBox[5] = _activeTM->getBoundingBox()[5];
    }
}
////////////////////////////////////////////////
void TextureBasedVizHandler::PreFrameUpdate()
{
    //if ( ModelHandler::instance()->GetActiveModel() )
    /*{
        if( CommandManager::instance()->GetXMLCommand() )
        {
            std::map<std::string, ves::xplorer::event::TextureBasedEventHandler*>::iterator currentEventHandler;
            const ves::open::xml::CommandPtr tbvizCommand = CommandManager::instance()->GetXMLCommand();
            currentEventHandler = _eventHandlers.find( tbvizCommand->GetCommandName() );
            if( currentEventHandler != _eventHandlers.end() )
            {
                vprDEBUG( vesDBG, 2 ) << "|\tExecuting: " << tbvizCommand->GetCommandName()
                << std::endl << vprDEBUG_FLUSH;
                currentEventHandler->second->SetGlobalBaseObject();
                currentEventHandler->second->Execute( tbvizCommand );
                _updateGraph();
            }
        }
    }*/
    _updateShaders();
}
////////////////////////////////////////////////////////////////////////////////
cfdPBufferManager* TextureBasedVizHandler::GetPBuffer()
{
    if( _pbm )
    {
        return _pbm;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::_updateShaderState()
{
    //first check which option is active
    /*if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ADVECTION_SHADER){
    if(_vvvh){
    cfdOSGAdvectionShaderManager* aShader = _vvvh->GetAdvectionShaderManager();
    if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == DYE_TRANSLATION){
    float dyeTranslation[3] = {0.0,0.0,0.0};
    dyeTranslation[0] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_SC);
    dyeTranslation[0] /= 100.0;
    dyeTranslation[1] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_MIN);
    dyeTranslation[1] /= 100.0;
    dyeTranslation[2] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_MAX);
    dyeTranslation[2] /= 100.0;
    aShader->UpdateDyeTranslation(dyeTranslation);
    }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == NOISE_SCALE){
    //uniform scaling
    //this will change to allow seperate scaling of injeciton materials
    float noiseScale[3] = {1.0,1.0,1.0};
    noiseScale[0] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_SC);
    noiseScale[0] /= 100.0;
    noiseScale[1] = noiseScale[0];
    noiseScale[2] = noiseScale[0];
    aShader->UpdateNoiseScale(noiseScale);
    }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == WEIGHT){
    float weights[2] = {.8,.2};
    weights[0] = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_SC) );
    weights[0] /= 100.0;
    weights[1] = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_MIN) );
    weights[1] /= 100.0;
    float whichMat = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_MAX) );
    aShader->UpdateWeight(weights, static_cast< int >( whichMat ) );
    }
    _vvvh->EnableDecorator();
    activeVisNodeHdlr = _vvvh;
    }
    }else if(activeVisNodeHdlr && _activeTM){
    if(!activeVisNodeHdlr->IsThisActive()){
    activeVisNodeHdlr->EnableDecorator();
    }
    }*/
}
////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdateScalarRange( float* range )
{
    if( _svvh )
    {
        cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
        if( sShader )
        {
            //sShader->FastTransferFunctionUpdate();
            sShader->FullTransferFunctionUpdate();
            sShader->DeactivateIsoSurface();
            sShader->SetScalarRange( range );
        }
        _activeVisNodeHdlr = _svvh;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::UpdatePreIntegrationTable( bool trueFalse )
{
    if( _svvh )
    {
        cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>( _svvh->GetActiveShader() );
        if( sShader )
        {
            //this is hard coded and will need to change--biv
            if( trueFalse )
            {
                sShader->FullTransferFunctionUpdate();
            }
            else
            {
                sShader->FastTransferFunctionUpdate();
            }
            sShader->EnsureScalarRange();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::PingPongTextures()
{
    if( _vvvh && _vvvh->IsThisActive() )
    {
        _vvvh->PingPongTextures();
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetPBuffer( cfdPBufferManager* pbm )
{
    if( _pbm != pbm )
    {
        _pbm = pbm;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetWorldDCS( ves::xplorer::scenegraph::DCS* dcs )
{
    if( _worldDCS != dcs )
    {
        _worldDCS = dcs;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetParentNode( ves::xplorer::scenegraph::Group* parent )
{
    if( _parent != parent )
    {
        LOG_INFO( "TextureBasedVizHandler::SetParentNode " << parent->getName() );
        _parent = parent;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetActiveTextureDataSet( cfdTextureDataSet* tds )
{
    if( tds != _activeTDSet )
    {
        LOG_INFO( "TextureBasedVizHandler::SetActiveTextureDataSet" );
        _activeTDSet = tds;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::SetActiveShaderManager( std::string name )
{
    if( _activeVisNodeHdlr )
    {
        if( name != "CUSTOM" )
        {
            _activeVisNodeHdlr->SetActiveShader( name );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::_updateScalarVisHandler()
{
    if( _activeTM && _activeVolumeVizNode )
    {
        if( !_svvh )
        {
            _svvh = new cfdScalarVolumeVisHandler();

        }
        _svvh->SetBoundingBox( _activeTM->getBoundingBox() );
        _svvh->SetSwitchNode( _activeVolumeVizNode->GetVolumeVisNode().get() );
        _svvh->SetTextureScale( _activeVolumeVizNode->GetTextureScale(), false );
        _svvh->SetCenter( _activeVolumeVizNode->GetBBoxCenter() );
        _svvh->SetAttachNode( _activeVolumeVizNode->GetDecoratorAttachNode().get() );
        _svvh->SetTextureManager( _activeTM );
        _svvh->Init();
        //_svvh->SetActiveShader("BLUE_RED_LINEAR_SHADER");
        _activeVisNodeHdlr = _svvh;
        if( !_svvh->IsThisActive() )
        {
            _svvh->EnableDecorator();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::_updateVectorVisHandler()
{
    if( _activeTM && _activeVolumeVizNode && _pbm )
    {
        if( !_vvvh )
        {
            _vvvh = new cfdVectorVolumeVisHandler();
            _vvvh->SetPBufferManager( _pbm );
        }
        _vvvh->SetBoundingBox( _activeTM->getBoundingBox() );
        _vvvh->SetSwitchNode( _activeVolumeVizNode->GetVolumeVisNode().get() );
        _vvvh->SetTextureScale( _activeVolumeVizNode->GetTextureScale(), false );
        _vvvh->SetCenter( _activeVolumeVizNode->GetBBoxCenter() );

        _vvvh->SetAttachNode( _activeVolumeVizNode->GetDecoratorAttachNode().get() );
        _vvvh->SetTextureManager( _activeTM );
        _vvvh->Init();
        _activeVisNodeHdlr = _vvvh;
        if( !_vvvh->IsThisActive() )
        {
            _vvvh->EnableDecorator();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* TextureBasedVizHandler::GetActiveVolumeVizNode()
{
    return _activeVolumeVizNode;
}
////////////////////////////////////////////////////////////////////////////////
// Can this function be removed????
cfdVolumeVisualization* TextureBasedVizHandler::GetVolumeVizNode( int whichModel )
{
    std::cout << whichModel << std::endl;
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void TextureBasedVizHandler::ViewTextureBasedVis( bool trueFalse )
{
    LOG_INFO( "TextureBasedVizHandler::ViewTextureBasedVis " << trueFalse );
    _textureBaseSelected = trueFalse;
    if( _activeVolumeVizNode )
    {
        _activeVolumeVizNode->GetVolumeVisNode()->setNodeMask( trueFalse );
    }
}
////////////////////////////////////////////////////////////////////////////////
