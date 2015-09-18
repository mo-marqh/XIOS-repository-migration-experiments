#ifndef __XIOS_REGISTRY_HPP__
#define __XIOS_REGISTRY_HPP__

#include "base_type.hpp"
#include "type.hpp"
#include "mpi.hpp"
#include "message.hpp"

namespace xios
{
/*!
  \class CRegistry
 This class is a registry database which store key with an associated value. Internaly the value is stored as a memory bloc
 and the key is a string. The registry can be gathered and merge between mpi process, broadcasted and read or wrote from a file
*/
  class CRegistry : virtual public CBaseType
  {
    public:

/** Constructor, the communicator is used for bcast or gather operation between MPI processes */
      CRegistry(const MPI_Comm& comm=MPI_COMM_WORLD) : communicator(comm) {}
      
/** Copy constructor */
      CRegistry(const CRegistry& reg) ;
      

/** insert a value associated to a key*/
      void setKey(const std::string& key, const CBaseType& value) {this->setKey(key,value); }

/** insert a value associated to a key*/
      template<typename T> void setKey(const std::string& key, const CType<T>& value) {this->setKey_(key,value); }


/** retreive a value from a key */
      void getKey(const std::string& key, CBaseType& value ) {this->getKey_(key,value); }

/** retreive a value from a key */
      template<typename T> void getKey(const std::string& key, CType<T>&  value ) {this->getKey_(key,value); }


/** query for an already inserted key */
      bool foundKey(const std::string& key) const ;
      
/** The registry is wrote into a memory buffer */
      bool toBuffer(CBufferOut& buffer) const ;

/** The registry is read from a memory buffer */
      bool fromBuffer(CBufferIn& buffer) ;

/** The registry is wrote to the file given by "filename". If the registry is empty no file is wrote */
      void toFile(const string& filename) ;

/** The registry is read from the file given by "filename". If no file exist, the registry remain empty */     
      void fromFile(const string& filename) ;

/** Merge the registry with an other. Existing keys in the current registry are not overwritten */         
      void mergeRegistry(const CRegistry& inRegistry) ;

/** Broadcast registry from the root process (rank 0) to the other processes of the communicator */    
      void bcastRegistry(void) ;

/** Gather registry to the root process (rank 0) from the other processes of the communicator */          
      void gatherRegistry(void) ;

/** Gather registry with a hierachical algorithm which avoid root process to get registries from whole processes of the communicator.
   Registry are merged two by two hirarchicaly. */
      void hierarchicalGatherRegistry(void) ;

/** Destructor */
       ~CRegistry() { reset() ; }

/** Unimplemented, do not use (need for CBaseType pure virtual class) */
      void fromString(const string& str) ;

/** Dump registry to a string (need for CBaseType pure virtual class)*/      
      string toString(void) const ;

/** Clone the registry (need for CBaseType pure virtual class)*/      
      CRegistry* clone(void) const { return new CRegistry(*this); }

/** return the size needed to bufferized the registry (need for CBaseType pure virtual class)*/        
      size_t size(void) const ;

/** return true if the registry is empty (need for CBaseType pure virtual class)*/ 
      bool isEmpty(void) const { return registry.empty(); }

/** Clean the registry and delete associated memory (need for CBaseType pure virtual class)*/ 
      void reset(void) ;

/** Set the prefix added systematicaly to the keys, with "::" as separator*/       
      void setPath(const string& str) { path=str+"::" ; }
       
    private:

/** insert a value associated to a key (internal use)*/
      void setKey_(const std::string& key, const CBaseType& value) ;

/** retreive a value from a key (internal use)*/
      void getKey_(const std::string& key, CBaseType& value ) ;
      
/** use internally for recursivity */
      void gatherRegistry(const MPI_Comm& comm) ;

/** use internally for recursivity */
      void hierarchicalGatherRegistry(const MPI_Comm& comm) ;


/** Prefix added systematicaly to the keys, with "::" as separator*/
      std::string path ;

/** Map containing registry, the key is a string type and the value is stored in a pair with the size
 *  of the memory bloc and the associated pointer*/      
      std::map<std::string,std::pair<size_t,char*> > registry ;

/** MPI communicator used for broadcast and gather operation */
      MPI_Comm communicator ;
  } ;

  inline CMessage& operator<<(CMessage& msg, CRegistry& registry)
  {
      msg.push(registry) ;
      return msg ;
  }

  inline CMessage& operator<<(CMessage& msg, const CRegistry& registry)
  {
      msg.push(registry) ;
      return msg ;
  }

}

#endif
