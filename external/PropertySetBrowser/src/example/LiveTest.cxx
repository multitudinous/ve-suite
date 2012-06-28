#include <QtGui/QApplication>

#include <PropertySetBrowser/GenericPropertyBrowser.h>
#include <PropertySetBrowser/PropertySet.h>
#include <PropertySetBrowser/MakeLive.h>
#include <eventmanager/EventManager.h>
#include <eventmanager/OptionalMacros.h>

#include <iostream>
#include <cmath>

void BoolSlot( const std::string& uuid, bool value )
{
    std::cout << "BoolSlot uuid=" << uuid << ", value=" << value << std::endl;
}

void IntSlot( const std::string& uuid, int value )
{
    std::cout << "IntSlot uuid=" << uuid << ", value=" << value << std::endl;
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);


    // Create a PropertySet with some test data, and make the properties live
    PropertySetBrowser::PropertySetPtr propertySet( new PropertySetBrowser::PropertySet );

    propertySet->AddProperty( "ABoolean", true, "A Boolean Value" );

    propertySet->AddProperty( "AnInteger", 44, "An Integer value" );


    // Make both of these properties "live", which means we let MakeLive
    // auto-create signals and register them with EventManager to indicate
    // when either of these properties changes. This is an alternative to
    // connecting directly to the change signal emitted by the property itself.
    // The advantage here is that we can use EventManager's pattern
    // matching abilities to connect the signals from multiple instances of
    // PropertySet to a single handler (per property type). For instance, if
    // we were to have 100 instances of our test PropertySet, we could connect
    // to the change singal for each and every "AnInteger" by simply requesting
    // a connection from EventManager to every signal matching the name,
    // "AnIntegerSignal" which we pass in below.
    PropertySetBrowser::MakeLiveBasePtr p = PropertySetBrowser::MakeLiveBasePtr(
         new PropertySetBrowser::MakeLive<bool>( propertySet->GetUUIDAsString(),
             propertySet->GetProperty("ABoolean"),
             "ABooleanSignal") );

    PropertySetBrowser::MakeLiveBasePtr q = PropertySetBrowser::MakeLiveBasePtr(
          new PropertySetBrowser::MakeLive<int>( propertySet->GetUUIDAsString(),
              propertySet->GetProperty("AnInteger"),
              "AnIntegerSignal") );


    // Request a connection to the two signals created by MakeLive above. Note
    // the % prepended to the signal name. This is necessary because MakeLive
    // adds a unique identifier to the front of the signals it registers so as
    // to prevent name clashes. The % is a wildcard that matches any sequence of
    // characters.
    eventmanager::ScopedConnectionList connections;
    CONNECTSIGNALS_STATIC( "%ABooleanSignal",
                            void( const std::string&, bool ),
                            &BoolSlot,
                            connections,
                            any_SignalType,
                            normal_Priority );

    CONNECTSIGNALS_STATIC( "%AnIntegerSignal",
                            void( const std::string&, int ),
                            &IntSlot,
                            connections,
                            any_SignalType,
                            normal_Priority );


    // Create, set up, and show a GenericPropertyBrowser which can display
    // the PropertySet
    PropertySetBrowser::GenericPropertyBrowser browserDisplay;
    browserDisplay.ParsePropertySet( propertySet );
    browserDisplay.resize( 350, 350 );
    browserDisplay.show();
    
    return a.exec();
}
