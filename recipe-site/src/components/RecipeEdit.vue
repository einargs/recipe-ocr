<template>
  <page-skeleton
    v-loading.fullscreen.lock="saving"
    element-loading-text="saving">
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item :to="{
        name: 'recipe-view',
        params: { id: $route.params.id },
      }">
        {{ recipe.name }}
      </el-breadcrumb-item>
      <el-breadcrumb-item>
      Edit
      </el-breadcrumb-item>
    </template>
    <el-form ref="form" :model="recipe">
      <el-form-item label="Name" prop="name"
        :rules="[{
          required: true,
          message: 'Please input recipe name',
          trigger: 'blur'
        }]">
        <el-input size="large" v-model="recipe.name" />
      </el-form-item>
      <el-form-item label="Tags" prop="tags">
        <el-tooltip class="tooltip" effect="dark"
          placement="bottom" content="comma separated list">
          <el-input size="large" v-model="recipe.tags" />
        </el-tooltip>
      </el-form-item>
    </el-form>
    <image-list
      ref="imageList"
      v-model="recipe.images" />
    <el-button
      type="primary"
      size="large"
      :disabled="saving"
      class="save-button"
      circle
      @click="save">
      <el-icon><i-mdi-content-save /></el-icon>
    </el-button>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import ImageList from "./ImageList.vue"
import { getRecipe, updateRecipe } from "../Api.js"

export default {
  name: "RecipeEdit",
  components: {PageSkeleton, ImageList},
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
  methods: {
    async save() {
      if (this.$data.saving) {
        return
      }
      this.$data.saving = true

      let isValid = false

      try {
        isValid = await this.$refs.form.validate()
      } catch (validationError) {
        ElMessage.error(Object.values(validationError).flat()[0].message)
      }

      if (isValid) {
        try {
          let files = await this.$refs.imageList.loadFiles()
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
          ElMessage.error("An error occured saving the recipe. Check your network")
        }
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
      .catch(err => {
        console.error(err)
        ElMessage.error("An error occured getting the recipe. Check your network.")
      })
  },
}
</script>

<style>
.save-button {
  font-size: 1.6em;
  /* Sets it to work based on the font-size of the icon */
  --el-button-size: auto;
  position: fixed;
  bottom: 32px;
  right: 32px;
}
</style>
