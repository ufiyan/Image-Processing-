//
// F# image processing functions.
//
// More details?
//
// Name: Sufiyan Ahmed Syed
// School: University of Illinois Chicago
// Date: 03/27/2024
//

//This module provides utility functions and image processing operations
// such as sepia, increase intensity, flip horizontal, rotate 180 degrees,
// and edge detection on images represented in PPM format.

namespace ImageLibrary

module Utils =
    /// Clamps a value to the 0-255 range. Used to ensure RGB values stay within valid range.
    let clamp value = max 0 (min value 255)

    /// Updates a pixel's color values with given scaling factors for each RGB component.
    /// Ensures that the result is clamped to valid RGB ranges.
    let updatePixel (r, g, b) (rf, gf, bf) =
        (clamp (int (rf * float r)), clamp (int (gf * float g)), clamp (int (bf * float b)))
    
    /// Calculates the Euclidean distance between two RGB colors, treating them as points in 3D space.
    /// Used for edge detection to determine color difference between pixels.
    let colorDistance (r1, g1, b1) (r2, g2, b2) =
        let dr = float (r1 - r2)
        let dg = float (g1 - g2)
        let db = float (b1 - b2)
        sqrt (dr * dr + dg * dg + db * db)


// Image processing operations
module Operations =
    open Utils

    /// Applies a sepia filter to an RGB pixel.
    /// The sepia tone effect is achieved by adjusting the RGB values based on predefined formulas.
    let sepiaFilter (r, g, b) =
        let newRed = 0.393 * float r + 0.769 * float g + 0.189 * float b
        let newGreen = 0.349 * float r + 0.686 * float g + 0.168 * float b
        let newBlue = 0.272 * float r + 0.534 * float g + 0.131 * float b
        (clamp (int newRed), clamp (int newGreen), clamp (int newBlue))

    /// Converts an entire image to sepia tone.
    /// Maps over each row and then each pixel within to apply sepia filter.
    let Sepia width height depth image = 
        image |> List.map (fun row -> row |> List.map sepiaFilter)

 //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //

   /// Increases the intensity of a selected RGB channel across an entire image.
    /// Uses 'updatePixel' with scaling factors based on the chosen channel and intensity.
    /// Preserves the original structure of the image.

    let IncreaseIntensity (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (intensity:double) (channel:char) =
        let (rf, gf, bf) = match channel with
            | 'r' -> (intensity, 1.0, 1.0)
            | 'g' -> (1.0, intensity, 1.0)
            | 'b' -> (1.0, 1.0, intensity)
            | _   -> (1.0, 1.0, 1.0) 

        image |> List.map (fun row -> 
            row |> List.map (fun pixel -> 
                updatePixel pixel (rf, gf, bf)
            )
        )    

/// Flips an image horizontally by reversing each row of pixels.
/// This operation simulates a mirror effect along the vertical axis.

    let FlipHorizontal (width:int) (height:int) (depth:int) (image:(int*int*int) list list) = 
        image |> List.map List.rev

/// Rotates an image by 180 degrees.
/// Achieved by reversing the order of rows and then reversing the order of pixels within each row.
    
    let Rotate180 (width:int) (height:int) (depth:int) (image:(int*int*int) list list) = 
        image 
        |> List.rev 
        |> List.map List.rev 

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
    let EdgeDetect (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (threshold:int) =
        let thresholdF = float threshold

        let isEdge (color1, color2Option) =
            match color2Option with
            | Some color2 -> if colorDistance color1 color2 > thresholdF then (0, 0, 0) else (255, 255, 255)
            | None -> (255, 255, 255) 

        let processRow row nextRow =
            row
            |> List.mapi (fun i color ->
                let rightNeighbor = if i < width - 1 then Some (List.nth row (i + 1)) else None
                let bottomNeighbor = if nextRow <> None then Some (List.nth nextRow.Value i) else None
                if isEdge (color, rightNeighbor) = (0, 0, 0) || isEdge (color, bottomNeighbor) = (0, 0, 0)
                then (0, 0, 0)
                else (255, 255, 255)
            )

        let processedImage =
            image
            |> List.mapi (fun i row ->
                if i < height - 1 then processRow row (Some (List.nth image (i + 1)))
                else [] 
            )
            |> List.filter (fun row -> row <> []) 

        processedImage |> List.map (fun row -> row |> List.take (width - 1))