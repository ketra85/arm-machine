package com.ketra85.ArmMachine

case class MMU(emulator: Emulator) {

  var memory = List.fill[Byte](65536)(0)

  def write8(address: Int, data: Byte): Unit = {
    memory(address) = data.toInt
  }

  def write16(address: Int, data: Short): Unit = {
    var addr: Int = address

    write8(addr, (data & 0xff).toByte)
    addr += 1

    write8(addr, ((data >> 8) & 0xff).toByte)
  }

  def write32(address: Int, data: Int): Unit = {
    var addr: Int = address

    write8(addr, (data & 0xff).toByte)
    addr += 1

    write8(addr, ((data >> 8) & 0xff).toByte)
    addr += 1

    write8(addr, ((data >> 16) & 0xff).toByte)
    addr += 1

    write8(addr, ((data >> 24) & 0xff).toByte)
  }

  def read8(address: Int): Byte = {
    memory(address)
  }

  def read16(address: Int): Short = {
    var ret: Short = 0
    var addr: Int = address

    var readByte: Byte = read8(addr)
    ret = readByte.toShort
    addr += 1

    readByte = read8(addr)
    ret |= readByte.toShort << 8
    ret
  }

  def read32(address: Int): Int = {
    var ret: Int = 0
    var addr: Int = address - (address % 4)

    var readByte: Byte = read8(addr)
    ret = readByte.toInt
    addr += 1

    readByte = read8(addr)
    ret |= readByte.toInt << 8
    addr += 1

    readByte = read8(addr)
    ret |= readByte.toInt << 16
    addr += 1

    readByte = read8(addr)
    ret |= readByte.toInt << 24

    if(address % 4 != 0) ret = (ret >> 8 * (address % 4)) | (ret << ((Int.MaxValue) * 8 - (8 * (address % 4))))
    ret
  }

}
