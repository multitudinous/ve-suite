#ifndef _PROPERTYSET_H
#define	_PROPERTYSET_H

#include <map>
#include <string>
#include <vector>

#include <boost/any.hpp>
#include <Poco/Types.h>

// Forward declarations
namespace Poco
{
namespace Data
{
class Session;
} // namespace Data
} // namespace Poco

namespace ves
{
namespace xplorer
{
namespace data
{

// Forward declaration
class Property;

///
/// @class PropertySet PropertySet is a base class for collections of properties
/// containing methods to read and write values and attributes of
/// properties, as well as to get information about changes to other properties
/// that may occur when a property value or attribute is changed.
class PropertySet
{
public:
    typedef std::map< std::string, Property* > PropertyMap;
    typedef std::vector<std::string> PSVectorOfStrings;

    ///
    /// Constructor
    PropertySet( );

    ///
    /// Copy constructor
    PropertySet( const PropertySet& orig );

    ///
    /// Destructor
    virtual ~PropertySet( );

    ///
    /// Add a new property named propertyName with main value value to this set
    virtual void AddProperty( std::string propertyName,
                              boost::any value,
                              std::string uiLabel = "" );

    ///
    /// Returns true if this property set owns a property with name propertyName,
    /// false if not.
    virtual bool PropertyExists( std::string propertyName ) const;

    ///
    /// Returns a vector containing the identifying names of all properties
    /// contained in this set. Names are in the order in which they were added
    /// to the property set.
    virtual PSVectorOfStrings GetPropertyList( );

    ///
    /// Returns a pointer to the property identified by propertyName
    virtual Property* GetProperty( std::string propertyName );

    ///
    /// Returns the boost::any main value of the property identified by
    /// propertyName
    virtual boost::any GetPropertyValue( std::string propertyName ) const;

    ///
    /// Returns a vector containing the identifying names of all attributes
    /// owned by property propertyName
    virtual const PSVectorOfStrings GetPropertyAttributeList( std::string
                                                            propertyName )const;

    ///
    /// Returns boost::any value of the property attribute identified by
    /// attributeName and owned by property propertyName
    virtual boost::any GetPropertyAttribute( std::string propertyName,
                                             std::string attributeName );

    ///
    /// Returns true if the property with name propertyName is currently enabled.
    virtual bool GetPropertyEnabled( std::string propertyName ) const;

    ///
    /// Sets the main value of the property indentified by propertyName to value.
    virtual bool SetPropertyValue( std::string propertyName,
                                   boost::any value );

    ///
    /// Sets the property attribute given by attributeName of the property
    /// propertyName to value.
    virtual void SetPropertyAttribute( std::string propertyName,
                                       std::string attributeName,
                                       boost::any value );

    ///
    /// Sets the enabled status of the property identified by propertyName.
    /// If the second argument is true, the property is enabled. If the second
    /// argument is false, the property is disabled.
    virtual void SetPropertyEnabled( std::string propertyName,
                                     bool enabled );

    ///
    /// Returns a list containing names of properties that have undergone a
    /// state change of some sort due to the most recent operation on a
    /// property in this set.
    virtual PSVectorOfStrings GetChanges( );

    ///
    /// Clears the internal list of all property changes that have occurred
    /// recently. The maximum size of the internal list is equal to the number
    /// of properties in the set.
    virtual void ClearAccumulatedChanges( );

    ///
    /// Sets the name of the table this property set tries to read from
    /// and write to.
    virtual void SetTableName( std::string TableName );

    ///
    /// Returns the name of the table this property set reads from and writes
    /// to.
    virtual std::string GetTableName( );

    ///
    /// Sets the record ID for this property. This should generally be used only
    /// to identify this property set before asking it to read data with a 
    /// matching record ID from the database.
    virtual void SetRecordID( unsigned int id );

    ///
    /// Returns the record ID of this property set. The record ID is how this
    /// property set is distingushed from others of a similar type in the
    /// database.
    virtual long unsigned int GetRecordID();

    virtual bool LoadFromDatabase( Poco::Data::Session *session );
    virtual bool LoadFromDatabase( Poco::Data::Session *session, std::string TableName );
    virtual bool LoadFromDatabase( Poco::Data::Session *session, std::string TableName, Poco::UInt32 ID );
    virtual bool LoadFromDatabase( std::string DatabaseName );
    virtual bool LoadFromDatabase( std::string DatabaseName, std::string TableName );
    virtual bool LoadFromDatabase( std::string DatabaseName, std::string TableName, unsigned int ID );

    virtual bool WriteToDatabase( Poco::Data::Session *session );
    virtual bool WriteToDatabase( Poco::Data::Session *session, std::string TableName );
    virtual bool WriteToDatabase( std::string DatabaseName );
    virtual bool WriteToDatabase( std::string DatabaseName, std::string TableName );

    virtual bool DeleteFromDatabase( Poco::Data::Session *session );
    virtual bool DeleteFromDatabase( Poco::Data::Session *session, std::string TableName );
    virtual bool DeleteFromDatabase( std::string DatabaseName );
    virtual bool DeleteFromDatabase( std::string DatabaseName, std::string TableName );

protected:

    ///
    /// Slot that should be connected to any state-change signal emitted by any
    /// property in this set. This connection is taken care of in the 
    /// AddProperty method through a call to c_onnectChanges. Derived classes
    /// that override AddProperty should be sure to call _connectChanges if
    /// change accumulation is desired.
    /// ChangeAccumulator will accumulate a list of all properties
    /// that have undergone a change in value, attributes, or enabled state
    /// since the last call to GetChanges or the last call to
    /// ClearAccumulatedChanges
    virtual void ChangeAccumulator( Property* property );

    ///
    /// Internal function that connects default signals to the change accumulator
    virtual void _connectChanges( Property* property );

    ///
    /// Internal function that looks through the property set and builds an
    /// appropriate string for creating an sqlite table for storing the data
    /// contained in this property set. If the default function is not doing
    /// what you need, override this function to create a custom table.
    virtual std::string _buildColumnHeaderString();

    PropertyMap mPropertyMap; /// Map holding the collection of properties.
    PSVectorOfStrings mAccumulatedChanges;
    PSVectorOfStrings mPropertyList; /// Maintains a list of available properties
    // sorted by order of addition

    std::string mTableName;
    Poco::UInt32 mID;


private:
    //std::string _toString( int value );
    //std::string _toString( size_t value );
    //std::string _toString( long unsigned int value );
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _PROPERTYSET_H */

