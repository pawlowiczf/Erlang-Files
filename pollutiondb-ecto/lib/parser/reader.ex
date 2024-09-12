defmodule Reader do
  def loadData(fileName) do
    #
    file = File.read!(fileName)
    lines = String.split(file, "\n")
    lines
  end
end
