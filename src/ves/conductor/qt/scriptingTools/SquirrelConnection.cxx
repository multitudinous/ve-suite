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
#include <ves/conductor/qt/scriptingTools/SquirrelConnection.h>

#if SWITCHWIRE_HAVE_SQUIRREL
#include <ves/conductor/qt/scriptingTools/SquirrelUtilClasses.h>

#include <switchwire/squirrel/Events.h>
#include <switchwire/squirrel/SQStdMap.h>
#include <switchwire/squirrel/SQStdVector.h>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
SquirrelConnection::SquirrelConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SquirrelConnection::SquirrelConnection( const std::string& scriptText )
{
    runScript( scriptText );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::ExposeSignalSlotTypes( switchwire::SquirrelContext& sc )
{
    // Add signal and slot types to expose to script engine.
    // Naming scheme for signal types should mimic that in
    // ves/util/SimpleDataTypeSignalSignatures.h, but omitting "_type" at the
    // end. Non-void return types should be prepended to the name, e.g.
    // Bool_IntSignal to describe a signal with signature bool( int ).

    // For code cleanliness...
    using namespace switchwire;

    ExposeSignalType_0
            < void( ) >
            ( "VoidSignal", sc );
    ExposeSlotType_0
            < void( ) >
            ( "VoidSlot", sc );

    ExposeSignalType_1
            < void( const std::string& ) >
            ( "StringSignal", sc );
    ExposeSlotType_1
            < void( const std::string& ) >
            ( "StringSlot", sc );

    ExposeSignalType_2
            < void( const std::string&, const std::string& ) >
            ( "TwoStringSignal", sc );
    ExposeSlotType_2
            < void( const std::string&, const std::string& ) >
            ( "TwoStringSlot", sc );

    ExposeSignalType_3
            < void ( std::string const&, std::string const&, std::string const& ) >
            ( "ThreeStringSignal", sc );
    ExposeSlotType_3
            < void ( std::string const&, std::string const&, std::string const& ) >
            ( "ThreeStringSlot", sc );

    ExposeSignalType_2
            < void( const std::string&, const bool& ) >
            ( "StringBoolSignal", sc );
    ExposeSlotType_2
            < void( const std::string&, const bool& ) >
            ( "StringBoolSlot", sc );

    ExposeSignalType_2
            < void( const std::string&, int ) >
            ( "StringIntSignal", sc );
    ExposeSlotType_2
            < void( const std::string&, int ) >
            ( "StringIntSlot", sc );

    ExposeSignalType_1
            < void ( bool const& ) >
            ( "BoolSignal", sc );
    ExposeSlotType_1
            < void ( bool const& ) >
            ( "BoolSlot", sc );

    ExposeSignalType_1
            < void ( double const& ) >
            ( "DoubleSignal", sc );
    ExposeSlotType_1
            < void ( double const& ) >
            ( "DoubleSlot", sc );

    ExposeSignalType_2
            < void ( double const&, double const& ) >
            ( "TwoDoubleSignal", sc );
    ExposeSlotType_2
            < void ( double const&, double const& ) >
            ( "TwoDoubleSlot", sc );

    ExposeSignalType_3
            < void ( double const&, double const&, double const& ) >
            ( "ThreeDoubleSignal", sc );
    ExposeSlotType_3
            < void ( double const&, double const&, double const& ) >
            ( "ThreeDoubleSlot", sc );

    ExposeSignalType_1
            < void ( int const& ) >
            ( "IntSignal", sc );
    ExposeSlotType_1
            < void ( int const& ) >
            ( "IntSlot", sc );

    ExposeSignalType_1
            < void ( unsigned int const& ) >
            ( "UnsignedIntSignal", sc );
    ExposeSlotType_1
            < void ( unsigned int const& ) >
            ( "UnsignedIntSlot", sc );

    ExposeSignalType_2
            < void ( const bool, const std::vector< double >& ) >
            ( "BoolAndDoubleVectorSignal", sc );
    ExposeSlotType_2
            < void ( const bool, const std::vector< double >& ) >
            ( "BoolAndDoubleVectorSlot", sc );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::BindSpecialClasses()
{
    // Add any other special classes that need to be bound to the bottom of
    // this method

    // Add any other vector types required to the bottom of this block
    // MS Windows is unhappy with these vector bindings. We need a wrapper
    // class that hides more of the guts than what's currently in BindSQStdVector
    /*
    BindSQStdVector< int >( "IntVector" );
    BindSQStdVector< float >( "FloatVector" );
    BindSQStdVector< double >( "DoubleVector" );
    BindSQStdVector< std::string >( "StringVector" );
    */

    // Add any other map types require to the bottom of this block
    BindSQStdMap< std::string, std::string >( "StringStringMap" );

    Sqrat::RootTable().Bind( "VizPropertySet", Sqrat::Class< VizPropertySetWrapper >()
        .Func( "CreateNewFeature", &VizPropertySetWrapper::CreateNewFeature )
        .Func( "SetBoolPropertyValue", &VizPropertySetWrapper::SetBoolPropertyValue )
        .Func( "SetIntPropertyValue", &VizPropertySetWrapper::SetIntPropertyValue )
        .Func( "SetFloatPropertyValue", &VizPropertySetWrapper::SetFloatPropertyValue )
        .Func( "SetDoublePropertyValue", &VizPropertySetWrapper::SetDoublePropertyValue )
        .Func( "SetStringPropertyValue", &VizPropertySetWrapper::SetStringPropertyValue )
        .Func( "Save", &VizPropertySetWrapper::Save )
        .Func( "BulkSave", &VizPropertySetWrapper::BulkSave )
        .Func( "GetUUIDAsString", &VizPropertySetWrapper::GetUUIDAsString )
    );

    Sqrat::RootTable().Bind( "TweakStore", Sqrat::Class< TweakStore >()
        .Func( "OpenTransaction", &TweakStore::OpenTransaction )
        .Func( "CloseTransaction", &TweakStore::CloseTransaction )
    );

    Sqrat::RootTable().Bind( "Sleeper", Sqrat::Class< Sleeper >()
        .Func( "Sleep", &Sleeper::Sleep )
    );

    Sqrat::RootTable().Bind( "Logger", Sqrat::Class< Logger >()
        .Func( "info", &Logger::info )
        .Func( "notice", &Logger::notice )
        .Func( "warning", &Logger::warning )
        .Func( "error", &Logger::error )
    );

    {
        Sqrat::Table namespaceTable;

        Sqrat::Class< BaseEvent > eventClass;
        namespaceTable.Bind( "Event", eventClass );

        namespaceTable.Bind( "State", Sqrat::Class< BaseState >()
            .Func( "enter", &BaseState::_enter )
            .Func( "exit", &BaseState::_exit )
            .Func( "handleEvent", &BaseState::_handleEvent )
        );

        namespaceTable.Bind( "Context", Sqrat::Class< BaseContext >()
            .Func( "setInitialState", &BaseContext::setInitialState )
            .Func( "handleEvent", &BaseContext::handleEvent )
        );

        Sqrat::RootTable().Bind( "StateMachine", namespaceTable );
    }    
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::runScript( const std::string& scriptText )
{
    switchwire::SquirrelContext sc;
    //Sqrat::SqratVM vm;
    Sqrat::DefaultVM::Set( sc.GetVM().getVM() );

    ExposeSignalSlotTypes( sc );
    BindSpecialClasses();

    sc.LoadScriptFromText( scriptText );
    sc.ExecuteScript();

    /*
    try
    {
        Sqrat::Script script;
        script.CompileString( scriptText );
        script.Run();
    }
    catch( Sqrat::Exception& e )
    {
        //TODO: push these message out to a logger rather than stdout
        std::cout << "Sqrat exception: " << e.Message() << std::endl << std::flush;
        return;
    }
    catch( ... )
    {
        std::cout << "Unspecified Sqrat exception" << std::endl << std::flush;
        return;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
}} //ves::conductor

#endif
