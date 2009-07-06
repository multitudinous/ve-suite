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
#include <ves/xplorer/Model.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/ModelDatasetHandler.h>

#include <ves/xplorer/environment/cfdSound.h>
#include <ves/xplorer/environment/HeadPositionCallback.h>
#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/scenegraph/util/Attribute.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/TextTexture.h>

#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>

#include <ves/xplorer/util/cfdGrid2Surface.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADCreator.h>

#include <ves/open/xml/shader/ShaderCreator.h>

#include <osg/StateSet>
#include <ves/xplorer/volume/cfdTextureDataSet.h>
using namespace ves::xplorer::volume;

#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>
//#include <vpr/vprTypes.h>
#include <gmtl/Matrix.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>
#include <gadget/Type/PositionInterface.h>


#include <vtkUnstructuredGrid.h>
#include <vtkDataWriter.h>
#include <vtkZLibDataCompressor.h>
#include <vtkUnsignedCharArray.h>
#include <vtkDataSet.h>
#include <vtkTriangleFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkSTLWriter.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGridReader.h>

#include <osg/BlendFunc>

#include <boost/bind.hpp>

#include <fstream>
#include <sstream>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::util;

namespace ves
{
namespace xplorer
{
Model::Model( ves::xplorer::scenegraph::DCS* worldDCS )
    :
    m_datasetHandler( 0 ),
    m_cadHandler( 0 ),
    activeDataSet( 0 ),
    mirrorDataFlag( false ),
    _activeTextureDataSet( 0 )
{
    vprDEBUG( vesDBG, 1 ) << "|\tNew Model ! "
        << std::endl << vprDEBUG_FLUSH;

    _worldDCS = worldDCS;
    m_cadHandler = new ves::xplorer::ModelCADHandler( _worldDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
Model::~Model()
{
    //vprDEBUG(vesDBG,2) << "Model destructor"
    //                       << std::endl << vprDEBUG_FLUSH;


    /* for(std::map<std::string ,ves::xplorer::event::EventHandler*>::iterator itr = _eventHandlers.begin();
                                         itr != _eventHandlers.end(); itr++ )
     {
        delete itr->second;
        itr->second = 0;
     }
     _eventHandlers.clear();*/

    for( std::vector< DataSet* >::iterator iter = mVTKDataSets.begin(); 
        iter != mVTKDataSets.end(); )
    {
        //std::cout << "Deleteing " << ( *iter )->GetFileName() << std::endl;
        delete *iter;
        iter = mVTKDataSets.erase( iter );
    }

    //texture data cleanup
    /*TextureDataSetList::iterator tDataSet;
    for(tDataSet=mTextureDataSets.begin(); tDataSet!=mTextureDataSets.end();tDataSet++ )
    {
          delete *tDataSet;
    }*/
    for( unsigned int i = 0; i < mTextureDataSets.size(); i++ )
    {
        delete mTextureDataSets.at( i );
    }
    mTextureDataSets.clear();
    //vprDEBUG(vesDBG,2) << "deleting mTextureDataSets"
    //   << std::endl << vprDEBUG_FLUSH;

    //std::map<int,DataSet>::iterator foundPlugin;
    // Remove any plugins that aren't present in the current network
    /*for ( foundPlugin=transientDataSets.begin(); foundPlugin!=transientDataSets.end(); )
    {
          // When we clear the _plugin map will
          // loop over all plugins
          transientDataSets.erase( foundPlugin++ );
    }
    transientDataSets.clear();
    */
    /*
       // The following block is broken
       // It loops more than it should (ie, twice for a single dataset)
       // and seg faults trying to erase something that is not there
       for(VTKDataSetList::iterator itr = mVTKDataSets.begin();
                                      itr != mVTKDataSets.end(); )
       {
          vprDEBUG(vesDBG,2) << "erasing a Model"
                                 << std::endl << vprDEBUG_FLUSH;
          mVTKDataSets.erase( itr++ );
       }
    */

    //vprDEBUG(vesDBG,2) << "Model destructor finished"
    //                       << std::endl << vprDEBUG_FLUSH;
    _availableSounds.clear();

    if( m_cadHandler )
    {
        delete m_cadHandler;
        m_cadHandler = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::ModelCADHandler* Model::GetModelCADHandler()
{
    return m_cadHandler;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::ModelDatasetHandler* Model::GetModelDatasetHandler()
{
    return m_datasetHandler;
}
////////////////////////////////////////////////////////////////////////////////
void Model::PreFrameUpdate()
{
    vprDEBUG( vesDBG, 1 ) << "Model::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Model::CreateCfdDataSet( void )
{
    mVTKDataSets.push_back( new DataSet() );
    mVTKDataSets.back()->SetModel( this );
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetMirrorNode( ves::xplorer::scenegraph::Group* dataNode )
{
    if( !mirrorNode )
    {
        mirrorNode = new ves::xplorer::scenegraph::Clone();
        mirrorNode->CloneNode( GetActiveDataSet()->GetDCS() );
        double rot[ 3 ];
        rot[ 0 ] = 180.0f;
        rot[ 1 ] = 0.0f;
        rot[ 2 ] = 0.0f;
        mirrorNode->SetRotationArray( rot );
        this->_worldDCS->AddChild( mirrorNode->GetClonedGraph() );
    }
    else
    {
        this->_worldDCS->RemoveChild( mirrorNode->GetClonedGraph() );
        delete mirrorNode;
        mirrorNode = new ves::xplorer::scenegraph::Clone( GetActiveDataSet()->GetDCS() );
        double rot[ 3 ];
        rot[ 0 ] = 180.0f;
        rot[ 1 ] = 0.0f;
        rot[ 2 ] = 0.0f;
        mirrorNode->SetRotationArray( rot );
        this->_worldDCS->AddChild( mirrorNode->GetClonedGraph() );
    }
}
////////////////////////////////////////////////////////////////////////////////
DataSet* Model::GetActiveDataSet( void )
{
    return activeDataSet;
}

////////////////////////////////////////////////////////////////////////////////
void Model::SetActiveDataSet( DataSet* input )
{
    activeDataSet = input;
}
////////////////////////////////////////////////////////////////////////////////
void Model::CreateTextureDataSet()
{
    // Currently we only have the capability to work with 1 texture dataset
    // per Model. This can change once the Visualization GUI is capable of
    // working with multiple texture datasets. Once that is possible
    // we will need a map of texture datasets organized by the overall
    // vtk dataset UUID.
    if( mTextureDataSets.size() > 0 )
    {
        return;
    }
    // Add a new texture dataset
    mTextureDataSets.push_back( new cfdTextureDataSet() );
    // Map texture dataset to UUID of overall vtk dataset
}
////////////////////////////////////////////////////////////////////////////////
void Model::AddDataSetToTextureDataSet( unsigned int index,
                                        std::string textureDescriptionFile )
{
    mTextureDataSets.at( index )->CreateTextureManager( textureDescriptionFile );
}
////////////////////////////////////////////////////////////////////////////////
void Model::CreateGeomDataSet( std::string filename )
{
    mGeomDataSets.push_back( new CADEntity( filename, _worldDCS.get(), false, true ) );
}
////////////////////////////////////////////////////////////////////////////////
void Model::setModelType( ModelTypeIndex type )
{
    this->mModelType = type;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* Model::GetDCS( )
{
    return this->_worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
bool Model::GetMirrorDataFlag( void )
{
    return mirrorDataFlag;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetMirrorDataFlag( bool input )
{
    mirrorDataFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& Model::GetID()
{
    return modelID;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetID( const std::string& id )
{
    modelID = id;
}
////////////////////////////////////////////////////////////////////////////////
DataSet* Model::GetCfdDataSet( int dataset )
{
    // Check and see if we have any datasets
    // if not return null
    // to get the last added dataset pass in -1
    if( mVTKDataSets.empty() )
        return NULL;
    else if( dataset == -1 )
        return mVTKDataSets.back();
    else
        return mVTKDataSets.at( dataset );
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Model::GetIndexOfDataSet( std::string dataSetName )
{
    unsigned int dataSetIndex = 0;
    for( size_t i = 0; i < mVTKDataSets.size(); ++i )
    {
        if( mVTKDataSets.at( i )->GetFileName() == dataSetName )
        {
            dataSetIndex = i;
            break;
        }
    }
    return dataSetIndex;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::volume::cfdTextureDataSet* Model::GetTextureDataSet( unsigned int index )
{
    if( mTextureDataSets.empty() )
    {
        return 0;
    }
    else
    {
        return mTextureDataSets.at( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetActiveTextureDataSet( ves::xplorer::volume::cfdTextureDataSet* tDS )
{
    _activeTextureDataSet = tDS;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::volume::cfdTextureDataSet* Model::GetActiveTextureDataSet()
{
    return _activeTextureDataSet;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Model::GetNumberOfTextureDataSets()
{
    return mTextureDataSets.size();
}
////////////////////////////////////////////////////////////////////////////////
int Model::GetKeyForCfdDataSet( DataSet* input )
{
    int key = -1;
    for( unsigned int i = 0; i < mVTKDataSets.size(); ++i )
    {
        if( mVTKDataSets.at( i ) == input )
        {
            key = i;
            break;
        }
    }

    return key;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Model::GetNumberOfCfdDataSets( void )
{
    return mVTKDataSets.size();
}
////////////////////////////////////////////////////////////////////////////////
CADEntity* Model::GetGeomDataSet( int dataset )
{
    // Check and see if we have any datasets
    // if not return null
    // to get the last added dataset pass in -1
    if( mGeomDataSets.empty() )
        return NULL;
    else if( dataset == -1 )
        return mGeomDataSets.back();
    else
        return mGeomDataSets.at( dataset );
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Model::GetNumberOfGeomDataSets( void )
{
    return mGeomDataSets.size();
}

////////////////////////////////////////////////////////////////////////////////
// Dynamic Loading Data Start From Here
void Model::DynamicLoadingData( vtkUnstructuredGrid* dataset, int datasetindex, double* scale, double* trans, double* rotate )
{
    this->CreateCfdDataSet();


    vprDEBUG( vesDBG, 0 ) << " ************************************* "
    << std::endl << vprDEBUG_FLUSH;

    vprDEBUG( vesDBG, 0 ) << " vtk DCS parameters:"
    << std::endl << vprDEBUG_FLUSH;

    //double scale[3], trans[3], rotate[3];   // pfDCS stuff
    //this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

    //hard code here and will change it later
    //scale[0]=2.0;scale[1]=2.0;scale[2]=2.0;
    //trans[0]=0.0; trans[1]=1.0; trans[2]=0.0;
    //rotate[0]=0.0; rotate[1]=0.0; rotate[2]=0.0;
    // Pass in -1 to GetCfdDataSet to get the last dataset added
    this->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
    this->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
    this->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );
    this->GetCfdDataSet( -1 )->LoadData( dataset, datasetindex );

    // this->MakingSurface( _model->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
    //
    std::cout << "[DBG]...Before add data into waitinglist" << std::endl;
    this->waitingdatalist.push_back( dataset );
    std::cout << "[DBG]...After add data into waitinglist" << std::endl;

}
////////////////////////////////////////////////////////////////////////////////
void Model::DynamicLoadingGeom( std::string surfacefilename, double* scale,
                                double* trans, double* rotate, double* stlColor, int color, int transFlag )
{
    std::cout << "[DBG]...the geom file is " << surfacefilename << std::endl;
    this->CreateGeomDataSet( surfacefilename );
    std::cout << "[DBG]...after cfdFile constructor" << std::endl;
    //this->GetGeomDataSet( -1 )->GetDCS()->SetScaleArray( scale );
    //this->GetGeomDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
    //this->GetGeomDataSet( -1 )->GetDCS()->SetRotationArray( rotate );
}
////////////////////////////////////////////////////////////////////////////////
std::vector<vtkDataSet*> Model::GetWaitingDataList()
{
    return this->waitingdatalist;
}
////////////////////////////////////////////////////////////////////////////////
#if __VJ_version > 2000003
void Model::GetDataFromUnit( void )
#elif __VJ_version == 2000003
void Model::GetDataFromUnit( void* unused )
#endif
{

//std::vector<VTKSmartPtr<vtkUnstructuredGrid> > _gridList;
    //char answer;

    vpr::Uint32 data_length = 0;
    vpr::Uint32 compressed_data_length( 0 );
    unsigned int bytes_read = 0;
    int i = 0;

    while( 1 )
    {
        std::cout << "[DBG]...Before ACCEPT New Data, *****************************************" << std::endl;
        vpr::SocketStream connection;
        vpr::InetAddr addr;
        std::string localhostname;
        localhostname = vpr::System::getHostname();
        addr.setAddress( localhostname, 50031 );
        vpr::SocketAcceptor server( addr );

        server.accept( connection );
        i++;

        // Get the length of the uncompressed data.
        mValueLock.acquire();
        {
            try
            {
                //connection.recvn( (void*)(&data_length), sizeof(data_length), bytes_read);
            }
            catch ( ... )
            {
                std::cerr << "[ERR] Unable to receive data length "
                << __FILE__ << ":" << __LINE__ << std::endl;
            }
        }
        mValueLock.release();
        // Get the length of the compressed data
        mValueLock.acquire();
        {
            try
            {
                //connection.recvn( (void*)(&compressed_data_length),
                //                         sizeof(compressed_data_length), bytes_read);
            }
            catch ( ... )
            {
                std::cerr << "[ERR] Unable to receive compressed data length "
                << __FILE__ << ":" << __LINE__ << std::endl;
                //return 1;
            }
        }
        mValueLock.release();

        ///Set the byte-order
        data_length = vpr::System::Ntohl( data_length );
        compressed_data_length = vpr::System::Ntohl( compressed_data_length );
        vpr::Uint8* compressed_data = new vpr::Uint8[compressed_data_length];
        mValueLock.acquire();
        {
            //connection.recvn( (void*)compressed_data,
            //                           compressed_data_length,
            //                           bytes_read );
        }
        mValueLock.release();

        /*if(status != vpr::ReturnStatus::Succeed)
        {
           std::cout << "[ERR] Error receiving data; read " << bytes_read << " of "
                     << data_length << " bytes." << std::endl;
           if (status == vpr::ReturnStatus::Fail)
           {
              std::cout << "[ERR] Read failed." << std::endl;
           }
           else if (status == vpr::ReturnStatus::WouldBlock)
           {
              std::cout << "[ERR] This read would block the caller." << std::endl;
           }
           else if (status == vpr::ReturnStatus::Timeout)
           {
              std::cout << "[ERR] This read timed out." << std::endl;
           }
           else if (status == vpr::ReturnStatus::InProgress)
           {
              std::cout << "[ERR] This read is still in progress." << std::endl;
           }
           else if (status == vpr::ReturnStatus::NotConnected)
           {
              std::cout << "[ERR] The device is not connected." << std::endl;
           }
           else
           {
              std::cout << "[ERR] Unknown Result." << std::endl;
           }
           //return 1;
         }*/
        std::cout << "[DBG] Read " << data_length << " of "
        << compressed_data_length << " bytes."
        << std::endl;
        // Uncompress the data
        vtkZLibDataCompressor* compressor = vtkZLibDataCompressor::New();
        vtkUnsignedCharArray* vtk_data = vtkUnsignedCharArray::New();
        vtk_data = compressor->Uncompress( compressed_data, compressed_data_length, data_length );
        /*VTKSmartPtr<vtkUnsignedCharArray> vtk_data =
                    compressor->Uncompress( compressed_data, 
                                            compressed_data_length,
                                            data_length );*/
        //VTKSmartPtr<vtkUnstructuredGridReader> reader;
        vpr::Uint8* data = new vpr::Uint8[vtk_data->GetSize()];
        data = vtk_data->WritePointer( 0, data_length );
        vtkUnstructuredGridReader* reader = vtkUnstructuredGridReader::New();
        reader->ReadFromInputStringOn();
        reader->SetBinaryInputString( reinterpret_cast<char*>( data ),
                                      data_length );


        //VTKSmartPtr<vtkUnstructuredGrid> ugrid(reader->GetOutput());

        vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
        ugrid = reader->GetOutput();
        mValueLock.acquire();
        {
            std::cout << "[DBG] Now drawing" << std::endl;
        }
        mValueLock.release();

        mValueLock.acquire();
        {
            std::cout << "[DBG]...READY TO READ DATA FROM UNIT **********************************" << std::endl;
            double scale[3], trans[3], rotate[3];
            scale[0] = scale[1] = scale[2] = 1.0f;
            trans[0] = trans[1] = trans[2] = 0.0f;
            rotate[0] = rotate[1] = rotate[2] = 0.0f;
            //since the user doesn't have access to the scale a and translate edata in this thread
            // this data for this cfddataset can be set after rloading that dataset
            // in the activate custom viz function when necessary
            this->DynamicLoadingData( ugrid, i, scale, trans, rotate );
            std::cout << "[DBG]...AFTER LOAD DATA ****************************************" << std::endl;
            this->currentsurfacefilename = ( std::string )( this->MakeSurfaceFile( ugrid, i ) );

            std::cout << "[DBG]...current surface file is " << currentsurfacefilename << std::endl;
            //currentsurfacefilename ="NewlyLoadedDataSet_1.stl";
            //this->DynamicLoadingGeom(currentsurfacefilename);
            std::cout << "[DBG]...After load geom data************************************" << std::endl;
            //reader->Delete();
            //ugrid->Delete();
            connection.close();

            std::cout << "[DBG]...After Server Close" << std::endl;
        }
        mValueLock.release();

    }

}
////////////////////////////////////////////////////////////////////////////////
void Model::ActiveLoadingThread()
{
#if __VJ_version > 2000003
    this->loadDataTh = new vpr::Thread( boost::bind( &Model::GetDataFromUnit, this ) );
#elif __VJ_version == 2000003
    this->loadDataTh = new vpr::Thread(
                           new vpr::ThreadMemberFunctor< Model >(
                               this, &Model::GetDataFromUnit ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string Model::MakeSurfaceFile( vtkDataSet* ugrid, int datasetindex )
{
    std::ostringstream file_name;
    std::string currentStlFileName = "NewlyLoadedDataSet_000.stl";
    file_name << "NewlyLoadedDataSet_" << datasetindex << ".stl";
    currentStlFileName = file_name.str();

    std::string newStlName;

    newStlName = currentStlFileName;

    vtkPolyData * surface = NULL;
    std::cout << "[DBG]... after readVtkThing" << std::endl;
    // Create a polydata surface file that completely envelopes the solution space
    float deciVal = 0.7;
    surface = cfdGrid2Surface( ugrid , deciVal );

    vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
    vtkGeometryFilter *gFilter = NULL;

    // convert dataset to vtkPolyData
    tFilter->SetInput(( vtkPolyData* )surface );

    std::cout << "Writing \"" << newStlName << "\"... ";
    std::cout.flush();
    vtkSTLWriter *writer = vtkSTLWriter::New();
    writer->SetInput( tFilter->GetOutput() );
    writer->SetFileName( newStlName.c_str() );
    writer->SetFileTypeToBinary();
    writer->Write();
    writer->Delete();
    std::cout << "... done" << std::endl;

    tFilter->Delete();

    if( gFilter )
        gFilter->Delete();

//clean up
    surface->Delete();
    std::cout << "\ndone\n";
    return newStlName;
}
//Dynamic Loading Data End Here
/////////////////////////////////////////////////
void Model::AddNewSound( std::string soundName,
                         std::string filename )
{
    cfdSound newSound;
    newSound.fileName = filename;
    newSound.soundName = soundName;

    if( newSound.initSound() )
    {
        _availableSounds[soundName] = newSound;
    }

}
////////////////////////////////////////////////////////////////////////////////
void Model::ActivateSound( std::string soundName )
{
    try
    {
        _availableSounds[soundName].playSound();
    }
    catch ( ... )
    {
        std::cout << "Invalid sound: " << soundName << std::endl;
        std::cout << "Model::ActivateSound" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::DeactivateSound( std::string soundName )
{
    try
    {
        _availableSounds[soundName].stopSound();
    }
    catch ( ... )
    {
        std::cout << "Invalid sound: " << soundName << std::endl;
        std::cout << "Model::ActivateSound" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::DeleteDataSet( std::string dataSetName )
{
    for( std::vector< DataSet* >::iterator iter = mVTKDataSets.begin();
            iter != mVTKDataSets.end(); ++iter )
    {
        if( (*iter)->GetFileName() == dataSetName )
        {
            vprDEBUG( vesDBG, 1 ) << "Deleting " << ( *iter )->GetFileName() 
                << std::endl << vprDEBUG_FLUSH;
            delete *iter;
            mVTKDataSets.erase( iter );
            break;
        }
        else
        {
            vprDEBUG( vesDBG, 1 ) << (*iter)->GetFileName() 
                << " is available not " << dataSetName 
                << std::endl << vprDEBUG_FLUSH;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::RenderTextualDisplay( bool onOff )
{
    if( onOff )
    {
        //add 3d blocks
        if( !mModelText.valid() )
        {
            mModelText = new ves::xplorer::scenegraph::TextTexture();
            
            osg::ref_ptr< ves::xplorer::scenegraph::DCS > textTrans = 
            new ves::xplorer::scenegraph::DCS();
            textTrans->addChild( mModelText.get() );
            
            _worldDCS->addChild( textTrans.get() );
            
            mModelText->setUpdateCallback( 
                                          new ves::xplorer::environment::TextTextureCallback( mModelText.get() ) );
            textTrans->setUpdateCallback( 
                                         new ves::xplorer::environment::HeadPositionCallback() );
            static_cast< osg::PositionAttitudeTransform* >( 
                mModelText->getParent( 0 ) )->setPosition(
                osg::Vec3d( 0, 0, 0 ) );
        }
        else
        {
            mModelText->setNodeMask( 1 );
        }
        
         
        std::string displayString = _worldDCS->getName() + "\n" + GetID();
        std::vector< std::string > filenames = 
        GetModelCADHandler()->GetCADFilenames();
        
        for( size_t i = 0; i < filenames.size(); ++i )
        {
            displayString = displayString + "\n" + filenames.at( i );
        }
        mModelText->UpdateText( displayString );
    }
    else
    {
        if( mModelText.valid() )
        {
            mModelText->setNodeMask( 0 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
} // end xplorer
} // end ves
