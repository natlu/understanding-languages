# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  All_My_Pieces = All_Pieces + [
    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), # additional shape 1
    [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # additional shape 2
     [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
    # rotations([[0, 0], [0, -1], [1, 0]]) # additional shape 3
    rotations([[0, 0], [-1, 0], [0, -1]])
  ]

  def self.next_cheat_piece (board)
    MyPiece.new(My_Cheat_Piece, board)
  end

  My_Cheat_Piece = [[[0, 0]]]

end


class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  # gets the next piece
  def next_piece
    if @cheat == false
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat = false
    end
    @current_pos = nil
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if !game_over? and @game.is_running?
      if @cheat == false and @score >= 100
        @score -= 100
        @cheat = true
      end
    end

  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end


class MyTetris < Tetris

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end

end


