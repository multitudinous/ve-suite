#include <QtGui/QApplication>

#include <PropertySetBrowser/GenericPropertyBrowser.h>
#include <PropertySetBrowser/PropertySet.h>

#include <iostream>
#include <cmath>

bool evenValidator( PropertySetBrowser::PropertyPtr property, boost::any value )
{
    // Only allow even numbers
    int castValue = boost::any_cast<int>(value);
    if( castValue % 2 == 0 )
    {
        std::cout << "Allowing value" << std::endl << std::flush;
        return true;
    }
    std::cout << "Disallowing value" << std::endl << std::flush;
    return false;
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);



    // Create a PropertySet with some test data
    PropertySetBrowser::PropertySetPtr propertySet( new PropertySetBrowser::PropertySet );

    propertySet->AddProperty( "ABoolean", true, "A Boolean Value" );

    propertySet->AddProperty( "ABoolean_Sub1", false, "Subvalue 1" );
    propertySet->AddProperty( "ABoolean_Sub2", 99, "Subvalue 2" );
    propertySet->AddProperty( "ABoolean_Sub3", 1.0134, "Subvalue 3" );

    propertySet->AddProperty( "AnInteger", 44, "An Integer value" );
    propertySet->SetPropertyAttribute( "AnInteger", "minimumValue", 41 );
    propertySet->SetPropertyAttribute( "AnInteger", "maximumValue", 50 );

    propertySet->AddProperty( "ADouble", 3.14, "A Double value" );

    propertySet->AddProperty( "AString", std::string("Hello World"), "A String Value" );

    propertySet->AddProperty( "AnEnum", std::string(), "An Enumerated Value" );
    std::vector< std::string > choices;
    choices.push_back("Option 1");
    choices.push_back("Option 2");
    choices.push_back("Option 3");
    propertySet->SetPropertyAttribute( "AnEnum", "enumValues", choices );

    propertySet->AddProperty( "AFilePath", std::string("Choose a file..."), "Path to some file" );
    propertySet->SetPropertyAttribute( "AFilePath", "isFilePath", true );

    propertySet->AddProperty( "IntWithValidation", 2, "Even-only integer" );
    propertySet->GetProperty( "IntWithValidation" )->SignalRequestValidation.connect( &evenValidator );
    // If the validator is a member method of a class, you have to use boost::bind
    // in the call to connect, like this:
    // propertySet->GetProperty( "IntWithValidation" )->
    //     SignalRequestValidation.connect(
    //     boost::bind( &MyClassName::MyMemberMethodName, this, _1, _2 ) );




    // Create, set up, and show a GenericPropertyBrowser which can display
    // the PropertySet
    PropertySetBrowser::GenericPropertyBrowser browserDisplay;
    browserDisplay.ParsePropertySet( propertySet );
    browserDisplay.resize( 350, 350 );
    browserDisplay.show();
    
    return a.exec();
}
