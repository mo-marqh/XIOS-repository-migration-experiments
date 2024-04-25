
#ifndef __BOOST_UUID_HPP__
#define __BOOST_UUID_HPP__

#include <random>
#include <climits>

struct boost_uuid
{
public:
    typedef uint8_t value_type;
    typedef uint8_t& reference;
    typedef uint8_t const& const_reference;
    typedef uint8_t* iterator;
    typedef uint8_t const* const_iterator;
    typedef std::size_t size_type;

    // This does not work on some compilers
    // They seem to want the variable definec in
    // a cpp file
    //BOOST_STATIC_CONSTANT(size_type, static_size = 16);
    static constexpr size_type static_size() { return 16; }

public:
    iterator begin() { return data; }
    const_iterator begin() const { return data; }
    iterator end() { return data+size(); }
    const_iterator end() const { return data+size(); }

    constexpr size_type size() const { return static_size(); }

public:
    // or should it be array<uint8_t, 16>
    uint8_t data[16];
};

unsigned long xios_random_long() {
    std::random_device rd;
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<unsigned long> dis(0, ULONG_MAX);
    return static_cast<unsigned long>(dis(gen));
}

boost_uuid gen_boost_uuid()
{
  boost_uuid u;

  int i=0;
  unsigned long random_value = xios_random_long();//generator();
  for (boost_uuid::iterator it=u.begin(); it!=u.end(); ++it, ++i) {
    if (i==sizeof(unsigned long)) {
      random_value = xios_random_long();//generator();
      i = 0;
    }

    // static_cast gets rid of warnings of converting unsigned long to boost::uint8_t
    *it = static_cast<boost_uuid::value_type>((random_value >> (i*8)) & 0xFF);
  }

  // set variant
  // must be 0b10xxxxxx
  *(u.begin()+8) &= 0xBF;
  *(u.begin()+8) |= 0x80;

  // set version
  // must be 0b0100xxxx
  *(u.begin()+6) &= 0x4F; //0b01001111
  *(u.begin()+6) |= 0x40; //0b01000000

  return u;
}


char to_char(size_t i) {
  if (i <= 9) {
    return static_cast<char>('0' + i);
  } else {
    return static_cast<char>('a' + (i-10));
  }
}

std::string to_string(boost_uuid const& u)
{
  std::string result;
  result.reserve(36);

  std::size_t i=0;
  for (boost_uuid::const_iterator it_data = u.begin(); it_data!=u.end(); ++it_data, ++i) {
    const size_t hi = ((*it_data) >> 4) & 0x0F;
    result += to_char(hi);

    const size_t lo = (*it_data) & 0x0F;
    result += to_char(lo);

    if (i == 3 || i == 5 || i == 7 || i == 9) {
      result += '-';
    }
  }
  return result;
}

#endif // __BOOST_UUID_HPP__
