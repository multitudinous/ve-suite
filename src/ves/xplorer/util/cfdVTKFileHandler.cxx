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
#include <ves/xplorer/util/cfdVTKFileHandler.h>
#include <vtkDataSet.h>
#include <vtkDataObject.h>
#include <vtkXMLFileReadTester.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkXMLRectilinearGridReader.h>
#include <vtkXMLImageDataReader.h>
#include <vtkImageData.h>
#include <vtkPolyDataReader.h>
#include <vtkDataSetReader.h>
#include <vtkDataReader.h>
#include <vtkStructuredGrid.h>
#include <vtkStructuredPoints.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkPolyData.h>
#include <vtkXMLDataSetWriter.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkStructuredGridWriter.h>
#include <vtkRectilinearGridWriter.h>
#include <vtkPolyDataWriter.h>
#include <iostream>
#include <vtkUnstructuredGridReader.h>

#ifdef VTK_POST_FEB20
#include <vtkXMLHierarchicalBoxDataReader.h>
#include <vtkXMLCompositeDataReader.h>
#include <vtkXMLCompositeDataWriter.h>
#include <vtkXMLMultiBlockDataWriter.h>
#include <vtkHierarchicalBoxDataSet.h>
#include <vtkXMLReader.h>
#include <vtkXMLWriter.h>
#include <vtkAlgorithm.h>
#else
#include <vtkXMLMultiGroupDataWriter.h>
#include <vtkHierarchicalDataSet.h>
#endif

#include <vtkXMLHierarchicalDataReader.h>
#include <vtkXMLMultiGroupDataReader.h>
#include <vtkXMLMultiBlockDataReader.h>
#include <vtkMultiBlockDataSet.h>
#include <vtkGenericDataObjectReader.h>
#include <vtkGenericDataObjectWriter.h>
#include <fstream>

using namespace ves::xplorer::util;

//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler():
    mDataReader( 0 ),
    _xmlTester( 0 ),
    _dataSet( 0 )
{
    _outFileType = CFD_XML;
    _outFileMode = CFD_BINARY;
}
/////////////////////////////////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler( const cfdVTKFileHandler& fh )
{
    _outFileType = fh._outFileType;
    _outFileMode = fh._outFileMode;

    _inFileName.assign( fh._inFileName );
    _outFileName.assign( fh._outFileName );

    _xmlTester = vtkXMLFileReadTester::New();
    _xmlTester = fh._xmlTester;
    _dataSet = fh._dataSet;
    std::cout << "cfdVTKFileHandler::cfdVTKFileHandler Bad News" << std::endl;
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdVTKFileHandler::~cfdVTKFileHandler()
{
    if( _xmlTester )
    {
        _xmlTester->Delete();
    }
}
////////////////////////////////////////////////////////////
void cfdVTKFileHandler::SetVTKOutFileType( OutFileType type )
{
    _outFileType = type;
}
/////////////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutFileWriteMode( OutFileMode mode )
{
    _outFileMode = mode;
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetInputFileName( std::string inFile )
{
    _inFileName = inFile;
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutputFileName( std::string oFile )
{
    _outFileName = oFile;
}
//////////////////////////////////////////////////////
/*vtkAlgorithm* cfdVTKFileHandler::GetAlgorithm()
{
    return mDataReader;
}*/
////////////////////////////////////////////////////////////////////
vtkDataObject* cfdVTKFileHandler::GetDataSetFromFile( const std::string& vtkFileName )
{
    if( vtkFileName.empty() )
    {
        return 0;
    }
    std::cout << "|\tLoading: " << vtkFileName << std::endl;
    _inFileName = vtkFileName;

    if( !_xmlTester )
    {
        _xmlTester = vtkXMLFileReadTester::New();
    }
    _xmlTester->SetFileName( _inFileName.c_str() );

    try
    {
        if( _xmlTester->TestReadFile() )
        {
            std::cout << "|\t\tXML ";
            std::cout << _xmlTester->GetFileDataType() << std::endl;
            //process xml file
            if( !std::strcmp( _xmlTester->GetFileDataType(), "UnstructuredGrid" ) )
            {
                _xmlTester->Delete();
                _xmlTester = 0;
                _getXMLUGrid();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "StructuredGrid" ) )
            {
                _getXMLSGrid();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "RectilinearGrid" ) )
            {
                _getXMLRGrid();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "PolyData" ) )
            {
                _getXMLPolyData();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "ImageData" ) )
            {
                GetXMLImageData();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "vtkMultiBlockDataSet" ) )
            {
                _getXMLMultiGroupDataSet();
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "vtkMultiGroupDataSet" ) )
            {
                _getXMLMultiGroupDataSet( false );
            }
            else if( !std::strcmp( _xmlTester->GetFileDataType(), "vtkHierarchicalDataSet" ) )
            {
                GetXMLHierarchicalDataSet();
            }
        }
        else
        {
            //this is a "classic" style vtk file
            _readClassicVTKFile();
        }
    }
    catch( ... )
    {
        std::cerr << "cfdVTKFileHandler::GetDataSetFromFile "
            << "Memory allocation error." << std::endl;
        _dataSet = 0;
    }
    return _dataSet;
}
/////////////////////////////////////////////
void cfdVTKFileHandler::_readClassicVTKFile()
{
    if( !_inFileName.c_str() )
        return;

    vtkGenericDataObjectReader* genericReader = vtkGenericDataObjectReader::New();
    genericReader->SetFileName( _inFileName.c_str() );
    genericReader->Update();

    {
        if( genericReader->IsFileUnstructuredGrid() )
        {
            std::cout << "|\t\tUnstructured Grid..." << std::endl;
            _dataSet = vtkUnstructuredGrid::New();
        }
        else if( genericReader->IsFileStructuredGrid() )
        {
            std::cout << "|\t\tStructured Grid..." << std::endl;
            _dataSet = vtkStructuredGrid::New();
        }
        else if( genericReader->IsFileRectilinearGrid() )
        {
            std::cout << "|\t\tRectilinear Grid..." << std::endl;
            _dataSet = vtkRectilinearGrid::New();
        }
        else if( genericReader->IsFilePolyData() )
        {
            std::cout << "|\t\tPolyData..." << std::endl;
            _dataSet = vtkPolyData::New();
        }
        else if( genericReader->IsFileStructuredPoints() )
        {
            std::cout << "|\t\tStructured Points..." << std::endl;
            _dataSet = vtkStructuredPoints::New();
        }
        else
        {
            std::cerr << "\nERROR - Unable to read this vtk file format"
                << std::endl;
            return ;
        }
        _dataSet->ShallowCopy( genericReader->GetOutput() );
        genericReader->Delete();
    }
}
/////////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLMultiGroupDataSet( bool isMultiBlock )
{
    vtkXMLCompositeDataReader* mgdReader = 0;
    if( isMultiBlock )
    {
        mgdReader = vtkXMLMultiBlockDataReader::New();
        _dataSet = vtkMultiBlockDataSet::New();
    }
    else
    {
        mgdReader = vtkXMLMultiGroupDataReader::New();
        _dataSet = vtkMultiBlockDataSet::New();
    }
    mgdReader->SetFileName( _inFileName.c_str() );
    mgdReader->Update();
    _dataSet->ShallowCopy( mgdReader->GetOutput() );
    mgdReader->Delete();
}
/////////////////////////////////////////////////
void cfdVTKFileHandler::GetXMLHierarchicalDataSet()
{
    vtkXMLHierarchicalDataReader* mgdReader = 0;
    mgdReader = vtkXMLHierarchicalDataReader::New();
    _dataSet = vtkHierarchicalBoxDataSet::New();
    mgdReader->SetFileName( _inFileName.c_str() );
    mgdReader->Update();
    _dataSet->ShallowCopy( mgdReader->GetOutput() );
    mgdReader->Delete();
}
//////////////////////////////////////
void cfdVTKFileHandler::_getXMLUGrid()
{
    vtkXMLUnstructuredGridReader* ugReader
        = vtkXMLUnstructuredGridReader::New();
    ugReader->SetFileName( _inFileName.c_str() );
    ugReader->Update();
    //_dataSet = ugReader->GetOutput();
    //_dataSet->Register( ugReader->GetOutput() );
    _dataSet = vtkUnstructuredGrid::New();
    _dataSet->ShallowCopy( ugReader->GetOutput() );
    ugReader->Delete();
}
//////////////////////////////////////
void cfdVTKFileHandler::_getXMLSGrid()
{
    vtkXMLStructuredGridReader* sgReader
    = vtkXMLStructuredGridReader::New();
    sgReader->SetFileName( _inFileName.c_str() );
    sgReader->Update();
    _dataSet = vtkStructuredGrid::New();
    _dataSet->ShallowCopy( sgReader->GetOutput() );
    sgReader->Delete();
}
/////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLRGrid()
{
    vtkXMLRectilinearGridReader* rgReader
    = vtkXMLRectilinearGridReader::New();
    rgReader->SetFileName( _inFileName.c_str() );
    rgReader->Update();
    _dataSet = vtkRectilinearGrid::New();
    _dataSet->ShallowCopy( rgReader->GetOutput() );
    rgReader->Delete();
}
////////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLPolyData()
{
    vtkXMLPolyDataReader* pdReader
    = vtkXMLPolyDataReader::New();
    pdReader->SetFileName( _inFileName.c_str() );
    pdReader->Update();
    _dataSet = vtkPolyData::New();
    _dataSet->ShallowCopy( pdReader->GetOutput() );
    pdReader->Delete();
}
////////////////////////////////////////////////
void cfdVTKFileHandler::GetXMLImageData( void )
{
    vtkXMLImageDataReader* reader = vtkXMLImageDataReader::New();
    reader->SetFileName( _inFileName.c_str() );
    reader->Update();
    _dataSet = vtkImageData::New();
    _dataSet->ShallowCopy( reader->GetOutput() );
    reader->Delete();
}
/////////////////////////////////////////////////////////////////////////////////
bool cfdVTKFileHandler::WriteDataSet( vtkDataObject* dataSet, std::string outFileName )
{
    if( outFileName.empty() )
    {
        return false;
    }
#ifdef VTK_POST_FEB20
    if( dataSet->IsA( "vtkMultiBlockDataSet" ) )
    {
        vtkXMLMultiBlockDataWriter* writer = vtkXMLMultiBlockDataWriter::New();
        writer->SetFileName( outFileName.c_str() );
        writer->SetInput( dataSet );
        if( _outFileMode == CFD_ASCII )
        {
            writer->SetDataModeToAscii();            
        }
        if( writer->Write() )
        {
            writer->Delete();
            return true;
        }
        writer->Delete();
        return false;
    }
#else
    if( dataSet->IsA( "vtkMultiGroupDataSet" ) )
    {
        vtkXMLMultiGroupDataWriter* writer = vtkXMLMultiGroupDataWriter::New();
        writer->SetFileName( outFileName.c_str() );
        writer->SetInput( dataSet );
        if( _outFileMode == CFD_ASCII )
        {
            writer->SetDataModeToAscii();
        }

        if( writer->Write() )
        {
            writer->Delete();
            return true;
        }
        writer->Delete();
        return false;
    }
#endif
    else
    {
        vtkXMLDataSetWriter* writer = vtkXMLDataSetWriter::New();
        writer->SetFileName( outFileName.c_str() );
        writer->SetInput( dynamic_cast<vtkDataSet*>( dataSet ) );
        if( _outFileMode == CFD_ASCII )
            writer->SetDataModeToAscii();

        if( writer->Write() )
        {
            writer->Delete();
            return true;
        }
        writer->Delete();
        return false;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////
void cfdVTKFileHandler::_writeClassicVTKFile( vtkDataObject * vtkThing,
                                              std::string vtkFilename, int binaryFlag )
{
    if( vtkThing == NULL )
    {
        std::cout << "Error: Can't write because vtkThing == NULL" << std::endl;
        return;
    }

    vtkThing->Update();

    std::cout << "Writing \"" << vtkFilename << "\", " << std::flush;

    if( vtkThing->IsA( "vtkUnstructuredGrid" ) )
    {
        std::cout << "a vtkUnstructuredGrid... " << std::flush;

        vtkUnstructuredGrid * uGrid = vtkUnstructuredGrid::SafeDownCast( vtkThing );
        if( uGrid == NULL )
        {
            std::cout << "SafeDownCast to a vtkUnstructuredGrid failed";
        }

        vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
        //writer->SetInput( (vtkUnstructuredGrid *)vtkThing );
        writer->SetInput( uGrid );
        writer->SetFileName( vtkFilename.c_str() );
        if( binaryFlag )
        {
            writer->SetFileTypeToBinary();
        }
        writer->Write();
        writer->Delete();
    }
    else if( vtkThing->IsA( "vtkStructuredGrid" ) )
    {
        std::cout << "a vtkStructuredGrid... " << std::flush;

        vtkStructuredGrid * sGrid = vtkStructuredGrid::SafeDownCast( vtkThing );
        if( sGrid == NULL )
        {
            std::cout << "SafeDownCast to a vtkStructuredGrid failed";
        }
        // Part of old vtk build may need to fix later
        //sGrid->BlankingOff();

        vtkStructuredGridWriter *writer = vtkStructuredGridWriter::New();
        writer->SetInput( sGrid );
        //writer->SetInput( (vtkStructuredGrid *)vtkThing );    //core dumped
        //writer->SetInput( vtkThing );                         //won't compile
        writer->SetFileName( vtkFilename.c_str() );
        if( binaryFlag )
        {
            writer->SetFileTypeToBinary();
        }
        writer->Write();
        writer->Delete();
        //sGrid->Delete();
    }
    else if( vtkThing->IsA( "vtkRectilinearGrid" ) )
    {
        std::cout << "a vtkRectilinearGrid... " << std::flush;
        vtkRectilinearGridWriter *writer = vtkRectilinearGridWriter::New();
        writer->SetInput(( vtkRectilinearGrid* )vtkThing );
        writer->SetFileName( vtkFilename.c_str() );
        if( binaryFlag )
        {
            writer->SetFileTypeToBinary();
        }
        writer->Write();
        writer->Delete();
    }
    else if( vtkThing->IsA( "vtkPolyData" ) )
    {
        std::cout << "a vtkPolyData... " << std::flush;
        vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
        writer->SetInput(( vtkPolyData* )vtkThing );
        writer->SetFileName( vtkFilename.c_str() );
        if( binaryFlag )
        {
            writer->SetFileTypeToBinary();
        }
        writer->Write();
        writer->Delete();
    }
    else
    {
        std::cerr << "\nERROR - Unsupported vtk file format: file not written"
        << std::endl;
        return;
    }

    std::cout << "... done\n" << std::endl;
}
////////////////////////////////////////////////////////////////////////
cfdVTKFileHandler& cfdVTKFileHandler::operator=( const cfdVTKFileHandler& fh )
{
    if( this != &fh )
    {
        _outFileType = fh._outFileType;
        _outFileMode = fh._outFileMode;

        _inFileName.clear();
        _outFileName.clear();

        _inFileName.assign( fh._inFileName );
        _outFileName.assign( fh._outFileName );

        _xmlTester = fh._xmlTester;
        _dataSet = fh._dataSet;
    }
    return *this;
}
