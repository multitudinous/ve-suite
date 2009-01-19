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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_HDF5_TO_VTK_H
#define CFD_HDF5_TO_VTK_H

#include <hdf5.h>
#include <hdf.h>
#include <mfhdf.h>
#include <vtkDataSet.h>
#include <vtkRectilinearGrid.h>
#include <vector>
//class to read HDF files
//Currently only reads David Clarkes files for his
//demos but should be relatively"easy" to extract data
//from the other file types.

class cfdHDFToVTK
{
public:
    cfdHDFToVTK();
    cfdHDFToVTK( char* inHDFFileName );
    cfdHDFToVTK( char* inHDFFileName, char* outputVTKDir );

    enum HDFFILETYPE{DCLARKE, HDF4, HDF5, OTHER};
    ~cfdHDFToVTK();

    void setType( HDFFILETYPE type )
    {
        _type = type;
    }
    void setInputHDFFile( char* inFile );
    void setOutputVTKDirectory( char* outDir );
    void setVerboseTranslationFlag( int flag )
    {
        _verbose = flag;
    }
    void viewGridBeforeWriting( int noYes = 1 )
    {
        _viewGridBeforeWriting = noYes;
    }
    void setVTKOutFileName( char* vtkOut );
    void writeAsciiOutput()
    {
        _asciiOut = 1;
    }

    //returns 1 if successful
    int translateFileToVTK();

    friend herr_t groupTraverserCallback( hid_t groupID,
                                          const char* memberName,
                                          void* operatorData );

protected:
    int _readDCHDFFile( char* inDCFile );
    int _copyData( float32* inData, double* outData, int* dimensions );
    void _createVelocityMagnitudeScalars();
    void _createMagneticMagnitudeScalars();
    void _createRectilinearVTKGrid();
    void _addCellDataToGrid( vtkDataSet* dSet,
                             char* name,
                             std::vector<double*> cellData,
                             int nComponents,
                             int nTuples,
                             int pointCellGrid = 0 );
    void _writeRectilinearCellDataToPointDataFile( vtkRectilinearGrid* cellGrid,
                                                   char* fileName,
                                                   int asciiFile = 1 );
    char* _inHDFFile;
    char* _outVTKDirectory;
    char* _outFile;
    int _verbose;
    int _asciiOut;
    HDFFILETYPE _type;
    int _numCells;

    void _traverseGroup( int group, const char* name );
    void _extractDatasetInfo( int datasetID, const char* name );

    std::vector<char*> _scalarNames;
    std::vector<char*> _vectorNames;
    int32 _dimensions[3];
    int _rank;
    int _maxrank;
    double* _ax;
    double* _ay;
    double* _az;
    float32* _bx;
    float32* _by;
    float32* _bz;
    double* _deltaAX;
    double* _deltaBX;
    double* _deltaAY;
    double* _deltaBY;
    double* _deltaAZ;
    double* _deltaBZ;
    double* _vX;
    double* _vY;
    double* _vZ;
    double* _b1;
    double* _b2;
    double* _b3;
    double* _pseudoGravPot;
    double* _density;
    double* _energyDensity;

    double* _velocityMag;
    double* _magneticMag;

    int _viewGridBeforeWriting;

};
#endif //CFD_HDF5_TO_VTK_H
