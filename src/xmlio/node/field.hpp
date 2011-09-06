#ifndef __XMLIO_CField__
#define __XMLIO_CField__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"
#include "functor.hpp"
#include "functor_type.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "declare_group.hpp"
#include "calendar_util.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CGrid;

   ///--------------------------------------------------------------

   // Declare/Define CFieldAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CField)
#  include "field_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CField)

   ///--------------------------------------------------------------
   class CField
      : public CObjectTemplate<CField>
      , public CFieldAttributes
   {
         /// friend ///
         friend class CFile;

         /// typedef ///
         typedef CObjectTemplate<CField>   SuperClass;
         typedef CFieldAttributes SuperClassAttribute;

      public :

         typedef CFieldAttributes RelAttributes;
         typedef CFieldGroup      RelGroup;

         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString & id);
         CField(const CField & field);       // Not implemented yet.
         CField(const CField * const field); // Not implemented yet.

         /// Accesseurs ///
               boost::shared_ptr<CField> getDirectFieldReference(void) const;
         const boost::shared_ptr<CField> getBaseFieldReference(void)   const;
         const std::vector<boost::shared_ptr<CField> > & getAllReference(void) const;

         boost::shared_ptr<CGrid> getRelGrid(void) const ;
         boost::shared_ptr<CFile> getRelFile(void) const ;

      public :

         StdSize getNStep(void) const;

         const date::CDuration & getFreqOperation(void) const;
         const date::CDuration & getFreqWrite(void) const;

         boost::shared_ptr<date::CDate> getLastWriteDate(void) const;
         boost::shared_ptr<date::CDate> getLastOperationDate(void) const;

         boost::shared_ptr<func::CFunctor> getFieldOperation(void) const;
         
         ARRAY(double, 1) getData(void) const;

         const StdString & getBaseFieldId(void) const;

         /// Mutateur ///
         void setRelFile(const boost::shared_ptr<CFile> _file);
         void incrementNStep(void);

         template <StdSize N>
            inline bool updateData
               (const date::CDate & currDate, const date::CDuration & timestep, const ARRAY(double, N) data);

         bool updateDataServer
               (const date::CDate & currDate, const std::deque<ARRAY(double, 1)> storedClient);

      public :

         /// Test ///
         bool hasDirectFieldReference(void) const;

         /// Traitements ///
         void solveRefInheritance(void);
         void solveGridReference(void);
         void solveOperation(void);

         virtual void fromBinary(StdIStream & is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

      private :

         /// Propriétés privées ///
         
         std::vector<boost::shared_ptr<CField> > refObject;
         boost::shared_ptr<CField> baseRefObject;
         boost::shared_ptr<CGrid>  grid ;
         boost::shared_ptr<CFile>  file;

         date::CDuration freq_operation, freq_write;

         StdSize nstep;
         boost::shared_ptr<date::CDate>    last_Write, last_operation;
         boost::shared_ptr<func::CFunctor> foperation;
         
         ARRAY(double, 1) data;

   }; // class CField

   ///--------------------------------------------------------------

   // Declare/Define CFieldGroup and CFieldDefinition
   DECLARE_GROUP(CField);

   } // namespace tree

   ///-----------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void);

   ///-----------------------------------------------------------------
} // namespace xmlioserver

#include "grid.hpp"

namespace xmlioserver {
namespace tree {

   template <StdSize N>
      bool CField::updateData(const date::CDate & currDate, const date::CDuration & timestep, const ARRAY(double, N) _data)
   {        
      const date::CDate opeDate      = *last_operation + freq_operation;
      const date::CDate writeDate    = *last_Write     + freq_write;       

//      std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
//      std::cout << "Champ : "     << this->getBaseFieldId() << std::endl;
//      std::cout << "CurrDate : "  << currDate  << std::endl;
//      std::cout << "opeDate : "   << opeDate   << " = " << *last_operation << " + " << freq_operation << std::endl;
//      std::cout	<< "writeDate : " << writeDate << " = " << *last_Write     << " + " << freq_write     << std::endl;
//      std::cout << "(opeDate <= currDate)   = " << std::boolalpha << (opeDate <= currDate)   << std::endl;
//      std::cout	<< "(writeDate <= currDate) = " << std::boolalpha << (writeDate <= currDate) << std::endl;   
   
       //std::cout << ">> " << currDate <<  " : Envoi de données " << this->getBaseFieldId() << std::endl;
      if (opeDate <= currDate)
      {
         //std::cout << "> " << currDate << ": Operation du champs" << this->getBaseFieldId() << std::endl;
         
         if (this->data->num_elements() != this->grid->storeIndex[0]->num_elements())
         {
            this->data->resize(boost::extents[this->grid->storeIndex[0] ->num_elements()]);
         }
            
         ARRAY_CREATE(input, double, 1, [this->data->num_elements()]);
         this->grid->inputField(_data, input);          
         (*this->foperation)(input);
         
         *last_operation = currDate;
//         std::cout << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl; 
      }
      
      if (writeDate < (currDate + freq_operation))
      {
         this->foperation->final();
         this->incrementNStep();
         *last_Write = writeDate;
//         std::cout << "(*last_Write = currDate) : " << *last_Write << " = " << currDate	<< std::endl;
         return (true);        
      }
//      std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
      return (false);
   };

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CField__
