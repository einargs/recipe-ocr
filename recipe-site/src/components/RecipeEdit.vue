<template>
  <page-skeleton>
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item :to="recipeSlug">
        {{ recipe.name }}
      </el-breadcrumb-item>
      <el-breadcrumb-item>
      Edit
      </el-breadcrumb-item>
    </template>
    <el-form ref="form" :model="recipe">
      <el-form-item label="Name">
        <el-input v-model="recipe.name" />
      </el-form-item>
      <el-form-item label="Tags">
        <el-tooltip class="tooltip" effect="dark"
          placement="bottom" content="comma separated list">
          <el-input v-model="recipe.tags" />
        </el-tooltip>
      </el-form-item>
    </el-form>
    <div class="image-list">
      <el-row v-for="(image, idx) in recipe.images" :key="image">
        <el-col :span="2" :md="{span: 2, offset: 2}">
          <div class="image-buttons">
            <el-button type="text" round
              @click="moveImage(idx, idx-1)"
              :disabled="idx == 0">
              <el-icon><i-ep-caret-top /></el-icon>
            </el-button>
            <el-button type="text" round @click="deleteImage(idx)">
              <el-icon><i-ep-delete-filled /></el-icon>
            </el-button>
            <el-button type="text" round
              @click="moveImage(idx, idx+1)"
              :disabled="idx == (recipe.images.length - 1)">
              <el-icon><i-ep-caret-bottom /></el-icon>
            </el-button>
          </div>
        </el-col>
        <el-col :span="22" :md="{span: 18, offset: 0}">
          <el-image :src="image.url" />
        </el-col>
      </el-row>
    </div>
    <el-upload
      class="image-upload"
      drag
      action="#"
      :auto-upload="false"
      :on-change="addNewImage"
      :show-file-list="false"
      >
      <el-icon class="el-icon--upload">
        <i-ep-upload-filled />
      </el-icon>
      <div class="el-upload__text">
        Drop file here or <em>click to upload</em>
      </div>
    </el-upload>
    <el-button
      type="primary"
      size="large"
      :loading="saving"
      class="save-button"
      circle
      @click="save">
      <el-icon v-if="!saving"><i-mdi-content-save /></el-icon>
    </el-button>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { getRecipe, updateRecipe } from "../Api.js"

async function getFile(imageFile) {
  if (!imageFile.file) {
    let response = await fetch(imageFile.url)
    return response.blob()
  } else {
    return imageFile.file
  }
}

export default {
  name: "RecipeEdit",
  components: {PageSkeleton},
  data() {
    return {
      saving: false,
      recipe: {
        name: "",
        tags: "",
        images: [],
      },
    }
  },
  computed: {
    recipeSlug() {
      return {
        name: "recipe-view",
        params: { id: this.$route.params.id }
      }
    },
  },
  methods: {
    addNewImage(file, fileList) {
      this.$data.recipe.images.push({
        url: URL.createObjectURL(file.raw),
        file: file.raw,
      })
    },
    deleteImage(idx) {
      this.$data.recipe.images.splice(idx, 1)
    },
    moveImage(from, to) {
      let images = this.$data.recipe.images
      let img = images[from]
      images[from] = images[to]
      images[to] = img
    },
    async save() {
      this.$data.saving = true
      try {
        let files = await Promise.all(this.$data.recipe.images.map(getFile))
        await updateRecipe(this.$route.params.id, {
          name: this.recipe.name,
          tags: this.recipe.tags,
          files,
        })
        this.$router.push({
          name: "recipe-view",
          params: { id: this.$route.params.id },
        })
      } catch (err) {
        console.error(err)
        ElMessage("An error occured saving the recipe. Check your network")
      }
      this.$data.saving = false
    },
  },
  created() {
    getRecipe(this.$route.params.id)
      .then(recipe => {
        this.$data.recipe.name = recipe.name
        this.$data.recipe.tags = recipe.tags.join(", ")
        this.$data.recipe.images = recipe.images
          .map(url => ({ url }))
      })
  },
}
</script>

<style>
.image-buttons {
  display: flex;
  flex-flow: column nowrap;
  align-items: center;
}
.image-buttons .el-button {
  margin: 0px;
}
.image-buttons .el-button .el-icon {
  font-size: 20px;
}
.save-button {
  font-size: 1.6em;
  position: fixed;
  bottom: 32px;
  right: 32px;
}
</style>
