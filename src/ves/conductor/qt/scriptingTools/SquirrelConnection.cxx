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

#include <ves/xplorer/device/GameControllerCallbacks.h>

#include <switchwire/squirrel/Events.h>
#include <switchwire/squirrel/SQStdMap.h>
#include <switchwire/squirrel/SQStdVector.h>

#include <gadget/Type/DigitalData.h>
#include <gadget/Type/HatData.h>

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
            < void ( float const& ) >
            ( "FloatSignal", sc );
    ExposeSlotType_1
            < void ( float const& ) >
            ( "FloatSlot", sc );

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

    ExposeSignalType_1
            < void( ves::xplorer::device::GameControllerCallbacks::ControlMode::Mode ) >
            ( "ControlModeSignal", sc );
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

    Sqrat::RootTable().Bind( "VizPropertySet", Sqrat::Class< VizPropertySetWrapper >( Sqrat::DefaultVM::Get(), "VizPropertySet" )
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

    Sqrat::RootTable().Bind( "TweakStore", Sqrat::Class< TweakStore >( Sqrat::DefaultVM::Get(), "TweakStore" )
        .Func( "OpenTransaction", &TweakStore::OpenTransaction )
        .Func( "CloseTransaction", &TweakStore::CloseTransaction )
    );

    Sqrat::RootTable().Bind( "Sleeper", Sqrat::Class< Sleeper >( Sqrat::DefaultVM::Get(), "Sleeper" )
        .StaticFunc( "Sleep", &Sleeper::Sleep )
    );

    Sqrat::RootTable().Bind( "Logger", Sqrat::Class< Logger >( Sqrat::DefaultVM::Get(), "Logger" )
        .Func( "Info", &Logger::Info )
        .Func( "Notice", &Logger::Notice )
        .Func( "Warning", &Logger::Warning )
        .Func( "Error", &Logger::Error )
    );

    /*{
        Sqrat::Table namespaceTable;

        Sqrat::Class< BaseEvent > eventClass( Sqrat::DefaultVM::Get(), "Event" );
        namespaceTable.Bind( "Event", eventClass );

        namespaceTable.Bind( "State", Sqrat::Class< BaseState >( Sqrat::DefaultVM::Get(), "State" )
            .Func( "OnEnter", &BaseState::_OnEnter )
            .Func( "OnExit", &BaseState::_OnExit )
            .Func( "OnEvent", &BaseState::_OnEvent )
        );

        namespaceTable.Bind( "Context", Sqrat::Class< BaseContext >( Sqrat::DefaultVM::Get(), "Context" )
            .Func( "SetInitialState", &BaseContext::SetInitialState )
            .Func( "HandleEvent", &BaseContext::HandleEvent )
        );

        Sqrat::RootTable().Bind( "StateMachine", namespaceTable );
    }*/

    {
        Sqrat::Enumeration digitalState;
        digitalState.Const( "OFF", gadget::DigitalState::OFF );
        digitalState.Const( "ON", gadget::DigitalState::ON );
        digitalState.Const( "TOGGLE_ON", gadget::DigitalState::TOGGLE_ON );
        digitalState.Const( "TOGGLE_OFF", gadget::DigitalState::TOGGLE_OFF );

        Sqrat::ConstTable().Enum( "DigitalState", digitalState );
    }

    {
        Sqrat::Enumeration hatState;
        hatState.Const( "UP", gadget::HatState::UP );
        hatState.Const( "DOWN", gadget::HatState::DOWN );
        hatState.Const( "LEFT", gadget::HatState::LEFT );
        hatState.Const( "RIGHT", gadget::HatState::RIGHT );
        hatState.Const( "CENTERED", gadget::HatState::CENTERED );

        Sqrat::ConstTable().Enum( "HatState", hatState );
    }

    {
        Sqrat::Enumeration controlMode;
        controlMode.Const( "NAV", ves::xplorer::device::GameControllerCallbacks::ControlMode::NAV );
        controlMode.Const( "UI", ves::xplorer::device::GameControllerCallbacks::ControlMode::UI );
        controlMode.Const( "USER_DEFINED", ves::xplorer::device::GameControllerCallbacks::ControlMode::USER_DEFINED );

        Sqrat::ConstTable().Enum( "ControlMode", controlMode );
    }

    typedef SynchronizedSignalReceiver< gadget::DigitalState::State > DigitalStateSynchronizedSignalReceiver_type;

    Sqrat::RootTable().Bind( "DigitalStateSynchronizedSignalReceiver",
                             Sqrat::Class< DigitalStateSynchronizedSignalReceiver_type >(
                                 Sqrat::DefaultVM::Get(),
                                 "SynchronizedSignalReceiver<gadget::DigitalState::State>"
                             )
        .Func( "ConnectToSignal", &DigitalStateSynchronizedSignalReceiver_type::ConnectToSignal )
        .Func( "Disconnect", &DigitalStateSynchronizedSignalReceiver_type::Disconnect )
        .Func( "Pending", &DigitalStateSynchronizedSignalReceiver_type::Pending )
        .Func( "Pop", &DigitalStateSynchronizedSignalReceiver_type::Pop )
    );

    typedef SynchronizedSignalReceiver< gadget::HatState::State > HatStateSynchronizedSignalReceiver_type;

    Sqrat::RootTable().Bind( "HatStateSynchronizedSignalReceiver",
                             Sqrat::Class< HatStateSynchronizedSignalReceiver_type >(
                                 Sqrat::DefaultVM::Get(),
                                 "SynchronizedSignalReceiver<gadget::HatState::State>"
                             )
        .Func( "ConnectToSignal", &HatStateSynchronizedSignalReceiver_type::ConnectToSignal )
        .Func( "Disconnect", &HatStateSynchronizedSignalReceiver_type::Disconnect )
        .Func( "Pending", &HatStateSynchronizedSignalReceiver_type::Pending )
        .Func( "Pop", &HatStateSynchronizedSignalReceiver_type::Pop )
    );

    typedef SynchronizedSignalReceiver< float > FloatSynchronizedSignalReceiver_type;

    Sqrat::RootTable().Bind( "FloatSynchronizedSignalReceiver",
                             Sqrat::Class< FloatSynchronizedSignalReceiver_type >(
                                 Sqrat::DefaultVM::Get(),
                                 "SynchronizedSignalReceiver<float>"
                             )
        .Func( "ConnectToSignal", &FloatSynchronizedSignalReceiver_type::ConnectToSignal )
        .Func( "Disconnect", &FloatSynchronizedSignalReceiver_type::Disconnect )
        .Func( "Pending", &FloatSynchronizedSignalReceiver_type::Pending )
        .Func( "Pop", &FloatSynchronizedSignalReceiver_type::Pop )
    );

    typedef SynchronizedSignalReceiver< std::string > StringSynchronizedSignalReceiver_type;

    Sqrat::RootTable().Bind( "StringSynchronizedSignalReceiver",
                             Sqrat::Class< StringSynchronizedSignalReceiver_type >(
                                 Sqrat::DefaultVM::Get(),
                                 "SynchronizedSignalReceiver<std::string>"
                             )
        .Func( "ConnectToSignal", &StringSynchronizedSignalReceiver_type::ConnectToSignal )
        .Func( "Disconnect", &StringSynchronizedSignalReceiver_type::Disconnect )
        .Func( "Pending", &StringSynchronizedSignalReceiver_type::Pending )
        .Func( "Pop", &StringSynchronizedSignalReceiver_type::Pop )
    );

    typedef SynchronizedSignalReceiver< bool > BoolSynchronizedSignalReceiver_type;

    Sqrat::RootTable().Bind( "BoolSynchronizedSignalReceiver",
                             Sqrat::Class< BoolSynchronizedSignalReceiver_type >(
                                 Sqrat::DefaultVM::Get(),
                                 "SynchronizedSignalReceiver<bool>"
                             )
        .Func( "ConnectToSignal", &BoolSynchronizedSignalReceiver_type::ConnectToSignal )
        .Func( "Disconnect", &BoolSynchronizedSignalReceiver_type::Disconnect )
        .Func( "Pending", &BoolSynchronizedSignalReceiver_type::Pending )
        .Func( "Pop", &BoolSynchronizedSignalReceiver_type::Pop )
    );

    Sqrat::RootTable().Bind( "PartManipulatorPropertySet",
                             Sqrat::Class< PartManipulatorPropertySetWrapper >(
                                 Sqrat::DefaultVM::Get(),
                                 "PartManipulatorPropertySet"
                             )
        .Func( "InitializeWithNodePath", &PartManipulatorPropertySetWrapper::InitializeWithNodePath )
        .Func( "GetTranslationX", &PartManipulatorPropertySetWrapper::GetTranslationX )
        .Func( "GetTranslationY", &PartManipulatorPropertySetWrapper::GetTranslationY )
        .Func( "GetTranslationZ", &PartManipulatorPropertySetWrapper::GetTranslationZ )
        .Func( "SetTranslationX", &PartManipulatorPropertySetWrapper::SetTranslationX )
        .Func( "SetTranslationY", &PartManipulatorPropertySetWrapper::SetTranslationY )
        .Func( "SetTranslationZ", &PartManipulatorPropertySetWrapper::SetTranslationZ )
        .Func( "GetRotationX", &PartManipulatorPropertySetWrapper::GetRotationX )
        .Func( "GetRotationY", &PartManipulatorPropertySetWrapper::GetRotationY )
        .Func( "GetRotationZ", &PartManipulatorPropertySetWrapper::GetRotationZ )
        .Func( "SetRotationX", &PartManipulatorPropertySetWrapper::SetRotationX )
        .Func( "SetRotationY", &PartManipulatorPropertySetWrapper::SetRotationY )
        .Func( "SetRotationZ", &PartManipulatorPropertySetWrapper::SetRotationZ )
        .Func( "Save", &PartManipulatorPropertySetWrapper::Save )
    );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::runScript( const std::string& scriptText )
{
    try
    {
        switchwire::SquirrelContext sc;
        //Sqrat::SqratVM vm;
        Sqrat::DefaultVM::Set( sc.GetVM().GetVM() );

        ExposeSignalSlotTypes( sc );
        BindSpecialClasses();

        sc.LoadScriptFromText( scriptText );
        sc.ExecuteScript();
    }
    //TODO: push these messages out to a logger than than stderr
    catch( Sqrat::Exception& e )
    {
        std::cerr << "Sqrat exception: " << e.Message() << std::endl << std::flush;
    }
    catch( ... )
    {
        std::cerr << "Unspecified Sqrat exception" << std::endl << std::flush;
    }
}
////////////////////////////////////////////////////////////////////////////////
}} //ves::conductor

#endif
