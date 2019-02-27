import java.io.File

import org.clapper.classutil.ClassFinder

import scala.collection.mutable

object PluginManager {
  val plugins = new mutable.HashMap[String, Instruction]()
  val pluginFolder = new File("plugins")

  val DEFAULT_COND_MODE = false
  val DEFAULT_EXTRA_MODE = false

  val args = new Array[Any](3)

  def init() {
    val classPath = List(".").map(new File(_))
    val finder = ClassFinder(classPath)
    val classes = finder.getClasses()
    val classMap = ClassFinder.classInfoMap(classes.iterator)
    val pluginsToLoad = ClassFinder.concreteSubclasses("Instruction", classMap)

    pluginsToLoad.foreach {
      pluginString =>

        args.update(0, pluginString.name)
        args.update(1, DEFAULT_COND_MODE)
        args.update(2, DEFAULT_EXTRA_MODE)

        val plugin = Class.forName(pluginString.name).getDeclaredConstructor(classOf[String], classOf[Boolean], classOf[Boolean]).newInstance(args.map(_.asInstanceOf[Object]): _*).asInstanceOf[Instruction]
        plugins += (plugin.name -> plugin)

    }
  }

//  def getPlugin(name: String): Instruction = {
//    if(plugins.isEmpty) {
//      init()
//    }
//    Class.forName(plugins(name).name).getConstructor().asInstanceOf[Instruction]
//  }

}
