/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TEXTURE_DATA_SET_H
#define CFD_TEXTURE_DATA_SET_H
/*!\file TextureDataSet.h
* TextureData API
*/

/*!\class ves::xplorer::volume::TextureDataInfo
*
*/
#ifdef _OSG

#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <ves/VEConfig.h>
namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdVolumeVisualization;
class cfdTextureManager;
class VE_TEXTURE_BASED_EXPORTS TextureDataInfo
{
public:
    TextureDataInfo();
    TextureDataInfo( const TextureDataInfo& tdi );
    ~TextureDataInfo();
    void SetName( std::string name );
    void SetTextureManager( cfdTextureManager* tm );

    std::string GetName();
    cfdTextureManager* GetTextureManager();
    TextureDataInfo& operator=( const TextureDataInfo& tdi );
protected:
    std::string _name;
    cfdTextureManager* _tm;
};


/*!\class ves::xplorer::volume::cfdTextureDataSet
*
*/
class VE_TEXTURE_BASED_EXPORTS cfdTextureDataSet
{
public:
    ///Constructor
    cfdTextureDataSet();
    ///Destructor
    virtual ~cfdTextureDataSet();

    enum DataType {SCALAR, VECTOR};

    ///Set the active scalar by name
    ///\param name The name of the desired active scalar
    void SetActiveScalar( std::string name );

    ///Set the active vector by name
    ///\param name The name of the desired active vector
    void SetActiveVector( std::string name );

    ///DERPICATED.\nSet the name of the text file describing the texture data
    ///\param name Full path to the text file describing the texture data
    void SetFileName( std::string name );

    ///Create a cfdTextureManager from data in the given directory
    ///\param textureDataDirectory The directory containing the texture data
    void CreateTextureManager( std::string textureDataDirectory );

    ///Add cfdTextureManager data to the scalar list
    ///\param scalarrData cfdTextureManager containing scalar data
    void AddScalarTextureManager( cfdTextureManager* scalarData );

    ///Add cfdTextureManager data to the vector list
    ///\param vectorData cfdTextureManager containing vector data
    void AddVectorTextureManager( cfdTextureManager* vectorData );

    ///Find a vector by name. Return -1 if not found.
    ///\param name The name of the vector to search for.r
    int FindVector( std::string name );

    ///Find a scalar by name. Return -1 if not found.
    ///\param name The name of the scalar to search for.
    int FindScalar( std::string name );

    ///Get the number of scalars in this texture dataset
    unsigned int NumberOfScalars();

    ///Get the number of vectors
    unsigned int NumberOfVectors();

    ///Return the name of the scalar data referenced at index
    ///\param index The position within  the scalar data
    std::string ScalarName( unsigned int index );

    ///Return the name of the vector data referenced at index
    ///\param index The position within  the vector data
    std::string VectorName( unsigned int index );

    ///Get the active data type
    DataType ActiveDataType();

    ///Get the active cfdTextureManager
    cfdTextureManager* GetActiveTextureManager();

    ///Get the cfdVolumeVisualizationNode
    cfdVolumeVisualization* GetVolumeVisNode();
protected:

    DataType _activeDataType;///<The active data type
    unsigned int _nScalars;///<The number of scalars
    unsigned int _nVectors;///<The number of vectors
    std::string _fileName;///<DEPRICATED: The name of the file describing the texture data.
    cfdVolumeVisualization* _volVisNode;///<The volume visualization rendering node
    cfdTextureManager* _activeTM;///<The active cfdTextureManager

    typedef std::vector<TextureDataInfo*> TextureDataList;

    std::vector<std::string> _scalarNames;///<Names of the available scalars
    std::vector<std::string> _vectorNames;///<Names of the available vectors

    TextureDataList _scalars;///<The list of TextureDataInfo for each available scalar
    TextureDataList _vectors;///<The list of TextureDataInfo for each available vector
};
}
}
}
#endif
#endif// CFD_TEXTURE_DATA_SET_H
