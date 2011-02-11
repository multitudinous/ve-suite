/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#pragma once
/*!\file UIPluginBase.h
UIPlugin API
*/
/*!\class UIPluginBase
*
*/
#define QT_NO_KEYWORDS

#include <ves/VEConfig.h>
#include <ves/open/xml/model/PortPtr.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/CommandPtr.h>
#include <vector>
#include <map>
#include <string>
#include <utility>

#include <QtCore/QObject>

namespace ves
{
namespace conductor
{
//class XMLDataBufferEngine;
//class UserPreferencesDataBuffer;;

namespace util
{
//class CORBAServiceList;
//class CADNodeManagerDlg;
//class SoundsPane;
//class DataSetLoaderUI;
}
}
}

typedef std::vector< ves::open::xml::model::PortPtr > PORT;

namespace ves
{
namespace conductor
{


class VE_GUIPLUGINS_EXPORTS UIPluginBase: public QObject
{
    Q_OBJECT
public:

    enum port_type
    {
        INPUT_PORT,
        OUTPUT_PORT
    };

    UIPluginBase();

    ///Default destructor for plugins
    virtual ~UIPluginBase();

    ///This is the ID to identify the module
    unsigned int GetID();

    //The load function of the module,
    ///unpack the input string and fill up the UI according to this
    ves::open::xml::model::ModelPtr GetVEModel( void );

    ///Set the ve model
    void SetVEModel( ves::open::xml::model::ModelWeakPtr tempModel );

    ///Sets the active model in Xplorer
    ///NOTE: Keep in mind that after a user submits a job that the active model
    ///in xplorer is set to NULL. This means that in some use cases the
    ///developer will have to set the active model manually from the derived
    ///plugin classes.
    ///\return Tell the user that the call to xplorer was successful
    void SetActiveModel( void );

    //??? Still needed ???
    ///allows user to set the image to be displayed on the icon
    //void SetImageIcon( std::string path, float rotation = 0.0f, int mirror = 0, float scale = 1.0f );

    //??? Still needed ???
    ///allows users creating new plugins and change the icon
    //void SetImage( QImage& image );

    ///Add port to the plugin
    void AddPort( unsigned int portType  );

    ///Delete selected port
    void DeletePort( ves::open::xml::model::PortPtr port );

    ///Handle all events to toggle graphics for plugin off/on in cad mode
    ///\param event WX Event for menu event
    void TogglePlugin( unsigned int );

    ///Makes the associated VEModel active in xplorer
    void ActivateAssociatedModel(  );

    ///Optimize all of the CAD
    void OnOptimizeCAD(  );

    //void SetNetwork( Network* network );

    ///Send the id of this plugin to xplorer so that it is active for all
    ///other xplorer events
    void SendActiveId();

    ///Make this plugin a hierarchy plugin
    //void SetAsHierarchy();

    void SetPluginType( const std::string& pluginType );

    ///Returns the number of input ports
    int GetNumInPorts();

    ///Returns vector of input ports
    virtual PORT GetInPorts();

    ///Returns number of output ports
    int GetNumOutPorts();

    ///Returns vector of output ports
    virtual PORT GetOutPorts();


    /// ???
    //virtual void Lock( bool lock );

    /// ???
    //virtual bool Has3Ddata();

public Q_SLOTS:
    void SetNumInputPorts( int num );
    void SetNumOutputPorts( int num );

protected:

    void RegistVar( std::string vname, long* var );
    void RegistVar( std::string vname, double* var );
    void RegistVar( std::string vname, std::string* var );
    void RegistVar( std::string vname, std::vector< long >* var );
    void RegistVar( std::string vname, std::vector< double >* var );
    void RegistVar( std::string vname, std::vector< std::string >* var );
    void RegistVar( std::string vname, std::vector< std::vector<std::string> >* var );

//    ///Check the active id against the plugin id
//    bool CheckID();

    ///Remove all the dialogs that were opened for this plugin
    //void RemovePluginDialogsFromCanvas();

    ///id for the plugin
    unsigned int m_id;

    ///Copy of the model element pointer
    ves::open::xml::model::ModelPtr m_veModel;
    ves::open::xml::model::ModelWeakPtr m_parentModel;

//    //data storage types
//    std::vector< wxString > v_desc;
//    std::vector< wxString > v_value;

    std::map<std::string, long* >                     _int;
    std::map<std::string, double* >                   _double;
    std::map<std::string, std::string* >              _string;
    std::map<std::string, std::vector<long>* >        _int1D;
    std::map<std::string, std::vector<double>* >      _double1D;
    std::map<std::string, std::vector<std::string>* > _string1D;
    std::map<std::string, std::vector< std::vector<std::string> >* > _string2D;


    ///The network event handler
    //ves::conductor::Network* m_network;

    std::string m_pluginType;
    std::string m_pluginName;

    ///Port data info
    std::vector< ves::open::xml::model::PortPtr > m_inputPorts;
    std::vector< ves::open::xml::model::PortPtr > m_outputPorts;

private:
};
}
}

